open Passe
open Common
open Lwt
open Result

module Log = (val Passe.Logging.log_module "fs")

module type Sig = sig
	include Fs_ext.Sig

	val write_file_s : t -> Path.t -> ?proof:Lock.proof -> write_instruction Lwt_stream.t
		-> (unit, write_error) result Lwt.t

	val write_file : t -> Path.t -> ?proof:Lock.proof -> string
		-> (unit, write_error) result Lwt.t

	val read_file_s : t -> ?proof:Lock.proof -> Path.t
		-> (Lock.proof -> (string, error) result Lwt_stream.t -> ('a, error) result Lwt.t)
		-> ('a, error) result Lwt.t

	val read_file : t -> Path.t
		-> (string, error) result Lwt.t
end

module type AtomicSig = functor(Fs:Fs_ext.Impl) -> sig
	val with_writable: Fs.t -> Fs.Path.t
		-> (Fs.Path.t -> (Fs.write_commit, Fs.write_error) result Lwt.t)
		-> (unit, Fs.write_error) result Lwt.t

	val readable:  Fs.t -> Fs.Path.t -> (Fs.Path.t, Fs.error) result Lwt.t
end

(* TODO: remove; use Kv_store instead *)
module Make (Fs:Fs_ext.FS)(Atomic:AtomicSig) : (Sig with type t = Fs.t) = struct
	module Fs = Fs_ext.Make(Fs)
	module Atomic = Atomic(Fs)
	include Fs
	type path = Path.t

	let locks = ref PathMap.empty

	let _with_lock : 'a. path -> ?proof:Lock.proof -> (Lock.proof -> 'a Lwt.t) -> 'a Lwt.t = fun path ?proof fn ->
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
			return_unit
		]

	let locked_atomic_write fs path ?proof fn = _with_lock ?proof path (fun proof ->
		Atomic.with_writable fs path (fn proof)
	)

	let locked_atomic_read fs path ?proof fn = _with_lock ?proof path (fun proof ->
		Atomic.readable fs path |> Lwt.bindr (fn proof)
	)

	let write_file_s fs path ?proof stream : (unit, write_error) result Lwt.t =
		locked_atomic_write fs path ?proof (fun _proof path ->
			let unix_path = Fs.Path.to_unix path in
			Log.debug (fun m->m "Writing file stream: %s" unix_path);
			Lwt_stream.fold_s (fun instruction result ->
				(return result) |> Lwt_r.bind (fun offset -> match instruction with
					| `Rollback ->
						(* result := `Rollback; *)
						Log.debug (fun m->m "aborted write at offset %d" offset);
						return (Error (`Rollback))
					| `Output chunk ->
						let size = String.length chunk in
						Log.debug (fun m->m "writing chunk of len %d to %s at offset %d" size unix_path offset);
						write fs unix_path offset (Cstruct.of_string chunk)
							|> Lwt.map (function
								| Ok () -> Ok (offset + size)
								| Error err -> Error (`fs err)
							)
				)
			) stream (Ok 0) |> Lwt.map (function
				| Ok (_:int) ->
					Log.debug (fun m->m "comitting file write: %s" unix_path);
					(Ok `Commit)
				| Error (`Rollback) -> (Ok `Rollback)
				| Error (`fs err) -> Error err
			)
		)

	let write_file fs path ?proof contents =
		write_file_s fs path ?proof (Lwt_stream.of_list [`Output contents])

	let read_file_s = fun fs ?proof path consumer ->
		locked_atomic_read fs ?proof path (fun proof readable_path ->
			match readable_path with
				| Error _ as read_err ->
					Lwt.return read_err
				| Ok readable_path ->
					let unix_path = Fs.Path.to_unix readable_path in
					Log.debug (fun m->m "Reading file stream: %a" Path.pp path);
					let offset = ref 0 in
					let eof = ref false in
					let max_chunk_size = 4096 in
					let stream = (Lwt_stream.from (fun () ->
						if !eof then (return None) else (
							Fs.read fs unix_path !offset max_chunk_size >>= (function
								| Error err ->
										eof := true;
										return (Some (Error err))
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
									let result = if chunks = []
										then None
										else (Some (Ok (String.concat "" chunks))) in
									return result
								)
						)
					)) in
					consumer proof stream
		)

	let read_file fs path =
		read_file_s fs path (fun _proof stream ->
			Lwt_stream.fold (fun (chunk:(string, error) result) (acc:(string, error) result) ->
				R.bind acc (fun acc ->
					chunk |> R.map ((^) acc)
				)
			) stream (Ok "")
		)

end
