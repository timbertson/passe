(* abstract type (like KV) abstracting ove Cloud_storage, Fs & KV *)
open Passe
open Common
open Lwt
open Result
module Log = (val Passe.Logging.log_module "kv_store")

type connector = ..

module type STATIC = sig
	type t

	val read : t -> Path.relative -> (string option, Error.t) result Lwt.t
end

module type Sig = sig
	include STATIC

	val read_for_writing : t
		-> Path.relative
		-> (Lock.proof -> (string option, Error.t) result -> 'a Lwt.t)
		-> 'a Lwt.t

	val write : t -> Path.relative -> ?proof:Lock.proof -> string -> (unit, Error.t) result Lwt.t

	val delete : t -> Path.relative -> (unit, Error.t) result Lwt.t

	val reconnect : t -> string -> t

	val connect : connector -> (t, Error.t) result
end

module Fs = struct
	type connector += Fs of Path.base
	type t = Path.base

	module Atomic = Fs_unix.Atomic
	module PathLock = Lock.Map(Path.Full)()

	let locked_atomic_write path ?proof fn = PathLock.acquire ?proof path (fun proof ->
		Atomic.with_writable path (fn proof)
	)

	let locked_atomic_read path ?proof fn = PathLock.acquire ?proof path (fun proof ->
		fn proof path
	)
	
	let read_for_writing
	: t -> Path.relative -> (Lock.proof -> (string option, Error.t) result -> 'a Lwt.t) -> 'a Lwt.t
	= fun t path consumer ->
		locked_atomic_read (Path.join t path) (fun proof readable_path ->
			let get_contents () =
				Log.debug (fun m->m "Reading file stream: %a" Path.pp_full readable_path);
				Lwt_result.catch (fun () ->
					Lwt_io.with_file Lwt_io.Input (Path.to_unix readable_path) (fun ch ->
						Lwt_io.read ch
					)
				) |> Lwt.map (function
					| Ok str -> Ok (Some str)
					| Error Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
					| Error other -> Error (Error.failure_of_exn other)
				)
			in
			get_contents () |> Lwt.bindr (consumer proof)
		)

	let read t path = read_for_writing t path (fun _ contents -> Lwt.return contents)

	let write t path ?proof contents : (unit, Error.t) result Lwt.t =
		let path = Path.join t path in
		Fs_unix.mkdir_p (Path.to_unix path |> Filename.dirname) |> Lwt.bindr (fun () ->
			locked_atomic_write path ?proof (fun _proof path ->
				let path_s = Path.to_unix path in
				Log.debug (fun m->m "Writing file: %s" path_s);
				Lwt_result.catch (fun () ->
					Lwt_io.with_file Lwt_io.Output path_s (fun ch ->
						Lwt_io.write ch contents
					)
				) |> Lwt_r.reword_error Error.failure_of_exn
			)
		)

	let delete t path =
		let path_s = Path.join t path |> Path.to_unix in
		let%lwt () = if%lwt Lwt_unix.file_exists path_s then Lwt_unix.unlink path_s in
		Lwt.return (Ok ())

	let reconnect : t -> string -> t = fun _ base -> Path.base base

	let connect = function
		| Fs base -> Ok base
		| _ -> Error (`Invalid "connect")
end
