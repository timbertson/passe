(* abstract type (like KV) abstracting ove Cloud_storage, Fs & KV *)
open Passe
open Common
open Lwt
open Result
module Log = (val Passe.Logging.log_module "kv_store")

module Types = struct
	type write_instruction = [ `Output of string | `Rollback ]
end

type connector = ..

module type Base = sig
	include module type of Types
	type t

	val read_for_writing : t -> Path.relative -> (Lock.proof ->
		((string, Error.t) result Lwt_stream.t option, Error.t) result -> 'a Lwt.t) -> 'a Lwt.t

	val write_s : t -> Path.relative -> ?proof:Lock.proof -> write_instruction Lwt_stream.t -> (unit, Error.t) result Lwt.t

	val delete : t -> Path.relative -> (unit, Error.t) result Lwt.t

	val reconnect : t -> string -> t
end

module type Sig = sig
	include Base
	val read : t -> Path.relative -> (string option, Error.t) result Lwt.t
	val write : t -> Path.relative -> string -> (unit, Error.t) result Lwt.t

	val read_s : t -> Path.relative -> ((string, Error.t) result Lwt_stream.t -> ('a, Error.t) result Lwt.t) -> ('a option, Error.t) result Lwt.t

	val connect : connector -> (t, Error.t) result
end

module Augment(T:Base) = struct
	open T

	let read_s fs path fn =
		read_for_writing fs path (fun _proof -> function
			| Ok (Some stream) -> fn stream |> Lwt_r.map Option.some
			| Ok None -> return (Ok None)
			| Error e -> return (Error e)
		)

	let read fs path =
		read_s fs path (fun stream ->
			Lwt_stream.fold (fun (chunk:(string, Error.t) result) (acc:(string, Error.t) result) ->
				R.bind acc (fun acc ->
					chunk |> R.map ((^) acc)
				)
			) stream (Ok "")
		)

	let write fs path contents =
		write_s fs path (Lwt_stream.of_list [`Output contents])
end

module Of_fs(Fs: Fs_ext.Augmented)(AtomicF: Fs_ext.AtomicSig) = struct
	type connector += Fs of Fs.t * Path.base
	module Impl = (struct
		include Types
		type t = (Fs.t * Path.base)

		module Atomic = AtomicF(Fs)
		module PathLock = Lock.Map(Path.Full)()

		let locked_atomic_write fs path ?proof fn = PathLock.acquire ?proof path (fun proof ->
			Atomic.with_writable fs path (fn proof)
		)

		let locked_atomic_read fs path ?proof fn = PathLock.acquire ?proof path (fun proof ->
			Atomic.readable fs path |> Lwt.bindr (fn proof)
		)

		let cast_write_err : Fs.write_error -> Error.t = function
			| `Is_a_directory -> `Invalid "Is_a_directory"
			| `Not_a_directory -> `Invalid "Not_a_directory"
			| other -> `Failed (pp_strf Fs.pp_write_error other)

		let cast_read_err : Fs.error -> Error.t = fun err -> cast_write_err (Fs.as_write_error err)

		let result_of_read_err : Fs.error -> ('a option, Error.t) result = fun err ->
			match err with
				| `No_directory_entry -> Ok None
				| other -> Error (cast_read_err other)

		let read_for_writing = fun (fs,base) path consumer ->
			let path = Path.join base path in
			let content_stream path =
				let unix_path = Path.to_unix path in
				Log.debug (fun m->m "Reading file stream: %a" Path.pp_full path);
				let offset = ref 0 in
				let errored = ref false in
				let max_chunk_size = 4096 in
				let get_chunk = (fun () ->
					if !errored then (return None) else (
						Fs.read fs unix_path !offset max_chunk_size
							|> Lwt.map (function
							| Error err ->
								errored := true;
								Some (Error (cast_read_err err))
							| Ok chunks ->
								let initial_offset = !offset in
								let chunks = chunks |> List.map (fun chunk ->
									offset := !offset + (Cstruct.len chunk);
									Cstruct.to_string chunk
								) in
								Log.debug (fun m->
									let len = !offset - initial_offset in
									m "read %d chunks from %a[%d] => %db" (List.length chunks) Path.pp_full path !offset len
								);
								if chunks = []
									then None
									else (Some (Ok (String.concat "" chunks)))
							)
					)
				) in
				Lwt_stream.from get_chunk
			in

			locked_atomic_read fs path (fun proof readable_path ->
				let payload = match readable_path with
					| Error read_err ->
						Lwt.return (result_of_read_err read_err)
					| Ok path -> (
						Fs.stat fs (Path.to_unix path) |> Lwt.map (fun stat ->
							stat
								|> R.map (fun _stat -> Some (content_stream path))
								|> R.bind_error result_of_read_err
						)
					)
				in
				payload |> Lwt.bindr (consumer proof)
			)

		type write_cancellation = [ `Rollback | `Fs of Fs.write_error ]

		let write_s (fs,base) path ?proof stream : (unit, Error.t) result Lwt.t =
			let path = Path.join base path in
			Fs.mkdir_p fs (Path.to_unix path |> Filename.dirname) |> Lwt_r.bind (fun () ->
				locked_atomic_write fs path ?proof (fun _proof path ->
					let path_s = Path.to_unix path in
					Log.debug (fun m->m "Writing file stream: %s" path_s);
					Lwt_stream.fold_s (fun instruction result ->
						(return result) |> Lwt_r.bind (fun offset -> match instruction with
							| `Rollback ->
								Log.debug (fun m->m "aborted write at offset %d" offset);
								return (Error (`Rollback))
							| `Output chunk ->
								let size = String.length chunk in
								Log.debug (fun m->m "writing chunk of len %d to %s at offset %d" size path_s offset);
								Fs.write fs path_s offset (Cstruct.of_string chunk)
									|> Lwt.map (function
										| Ok () -> Ok (offset + size)
										| Error err -> Error (`Fs err)
									)
						)
					) stream (Ok 0) |> Lwt.map (function
						| Ok (_:int) ->
							Log.debug (fun m->m "comitting file write: %s" path_s);
							(Ok `Commit)
						| Error (`Rollback) -> (Ok `Rollback)
						| Error (`Fs err) -> Error err
					)
				)
			) |> Lwt.map (R.reword_error cast_write_err)

		let delete (fs, base) path =
			Fs.destroy fs (Path.to_unix (Path.join base path))
			|> Lwt.map (R.reword_error cast_write_err)

		let reconnect : t -> string -> t = fun (fs, _) base -> (fs, Path.base base)
	end)

	include Impl
	include Augment(Impl)

	let connect = function
		| Fs (fs, base) -> Ok (fs, base)
		| _ -> Error (`Invalid "connect")
end
