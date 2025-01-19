open Passe
open Lwt
open Common

let rec mkdir_p path : unit Lwt.t =
	try%lwt
		Lwt_unix.mkdir path 0o640
	with
		| Unix.Unix_error (ENOENT, _, _) ->
			let parent = (Filename.dirname path) in
			assert (path <> parent);
			mkdir_p parent |> Lwt.bindr (fun () -> mkdir_p path)
;;

module Atomic = struct
	let destroy_if_exists path : (unit, Error.t) Lwt_r.t =
		Lwt_result.catch (fun () ->
			if%lwt Lwt_unix.file_exists path then Lwt_unix.unlink path
		) |> Lwt_r.reword_error (fun e -> Error.failure_of_exn e)

	let with_writable
		dest
		(fn : Path.full -> (unit, Error.t) Lwt_r.t)
	: (unit, Error.t) Lwt_r.t =
		let success = Ok () in
		let tmpname = Path.modify_filename (fun name -> name ^ ".tmp") dest in
		let dest_unix = Path.to_unix dest in
		let tmpname_unix = Path.to_unix tmpname in
		let cleanup result : (unit, Error.t) Lwt_r.t = result
			|> Lwt_r.and_then (fun () -> destroy_if_exists tmpname_unix) in

		let handle (result : (unit, Error.t) Result.t) : (unit, Error.t) Lwt_r.t =
			match result with
				| Error _ as e -> cleanup e
				| Ok () -> (
					try
						Unix.rename tmpname_unix dest_unix; return success
					with e -> return (Error (Error.failure_of_exn e))
				)
		in
		
		cleanup (Ok ()) |> Lwt.bindr (function
			| Ok () -> fn tmpname |> Lwt.bindr handle
			| Error e -> Lwt.return (Error e)
		)

		(* cleanup (Ok ()) |> Lwt.and_then (fun () -> fn tmpname |> Lwt.bindr handle) *)
end
