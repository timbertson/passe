(* abstract type (like KV) abstracting ove Cloud_storage, Fs & KV *)
open Passe
open Common
open Lwt
open Result
module Log = (val Passe.Logging.log_module "kv_store")

module Core = struct
	type error = [
		| `Invalid (* will never succeed *)
		| `Failed of string (* possibly-transitory error *)
	]
	let string_of_error : error -> string = Obj.magic

	type write_instruction = [ `Output of string | `Rollback ]
end

module type Sig = sig
	include module type of Core
	type t
	module Path: sig
		type t
		val pp : t Fmt.t
		val make : string list -> (t, error) result
	end

	(* simple read/write *)
	val read : t -> Path.t -> (string option, error) result Lwt.t
	val write : t -> Path.t -> string -> (unit, error) result Lwt.t

	(* advanced read/write *)
	val read_for_writing : t -> Path.t -> (Lock.proof ->
		((string, error) result Lwt_stream.t option, error) result -> 'a Lwt.t) -> 'a Lwt.t

	val read_s : t -> Path.t -> ((string, error) result Lwt_stream.t -> ('a, error) result Lwt.t) -> ('a option, error) result Lwt.t

	val write_s : t -> Path.t -> ?proof:Lock.proof -> write_instruction Lwt_stream.t -> (unit, error) result Lwt.t

	val delete : t -> Path.t -> (unit, error) result Lwt.t
	val connect : string -> t
end

module Of_fs(Fs: Fs_ext.Impl)(AtomicF: Filesystem.AtomicSig) : Sig = struct
	include Core
	type t = Fs.t
	module Path = struct
		type t = Fs.Path.t
		let pp = Obj.magic
		let make = Obj.magic
	end
	module Atomic = AtomicF(Fs)

	module PathMap = Fs.PathMap

	let locks = ref PathMap.empty

	let _with_lock : 'a. Path.t -> ?proof:Lock.proof -> (Lock.proof -> 'a Lwt.t) -> 'a Lwt.t = fun path ?proof fn ->
		let lock =
			try PathMap.find path !locks
			with Not_found -> begin
				let lock = Lock.create () in
				locks := PathMap.add path lock !locks;
				lock
			end in
		(try%lwt
			Lock.use ?proof lock fn with e -> raise e
		) [%lwt.finally
			Log.debug (fun m->m "finished with mutex %a; no_open_locks = %b" Path.pp path (Lock.is_empty lock));
			if (Lock.is_empty lock) then (
				locks := PathMap.remove path !locks
			);
			Lwt.return_unit
		]

	let locked_atomic_write fs path ?proof fn = _with_lock ?proof path (fun proof ->
		Atomic.with_writable fs path (fn proof)
	)

	let locked_atomic_read fs path ?proof fn = _with_lock ?proof path (fun proof ->
		Atomic.readable fs path |> Lwt.bindr (fn proof)
	)


	let cast_read_err : Fs.error -> error = function
		| `Is_a_directory -> `Invalid
		| `Not_a_directory -> `Invalid
		| other -> `Failed (pp_strf Fs.pp_error other)

	let cast_write_err : Fs.write_error -> error = function
		(* TODO: this repeats above, but I can't pull out the common vaiants from error + write_error *)
		| `Is_a_directory -> `Invalid
		| `Not_a_directory -> `Invalid
		| other -> `Failed (pp_strf Fs.pp_write_error other)

	let result_of_read_err : Fs.error -> ('a option, error) result = fun err ->
		match err with
			| `No_directory_entry -> Ok None
			| other -> Error (cast_read_err other)

	let read_for_writing = fun fs path consumer ->
		let content_stream path =
			let unix_path = Fs.Path.to_unix path in
			Log.debug (fun m->m "Reading file stream: %a" Path.pp path);
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
								m "read %d chunks from %a[%d] => %db" (List.length chunks) Path.pp path !offset len
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
				| Ok unix_path -> (
					Fs.stat fs unix_path |> Lwt.map (fun stat ->
						stat
							|> R.map (fun _stat -> Some (content_stream unix_path))
							|> R.bind_error result_of_read_err
					)
				)
			in
			payload |> Lwt.bindr (consumer proof)
		)

	let read_s fs path fn =
		read_for_writing fs path (fun _proof -> function
			| Ok (Some stream) -> fn stream |> Lwt_r.map Option.some
			| Ok None -> return (Ok None)
			| Error e -> return (Error e)
		)

	let read fs path =
		read_s fs path (fun stream ->
			Lwt_stream.fold (fun (chunk:(string, error) result) (acc:(string, error) result) ->
				R.bind acc (fun acc ->
					chunk |> R.map ((^) acc)
				)
			) stream (Ok "")
		)

	type write_cancellation = [ `Rollback | `Fs of Fs.write_error ]

	let write_s fs path ?proof stream : (unit, error) result Lwt.t =
		Fs.mkdir_p fs path |> Lwt_r.bind (fun () ->
			locked_atomic_write fs path ?proof (fun _proof path ->
				let path_s = Fs.Path.to_unix path in
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

	let write fs path contents =
		write_s fs path (Lwt_stream.of_list [`Output contents])

	let delete fs path =
		Fs.destroy fs (Fs.Path.to_unix path)
		|> Lwt.map (R.reword_error cast_write_err)

	let connect = Obj.magic
end
