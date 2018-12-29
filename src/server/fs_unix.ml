open Passe
open Lwt
open Common
open Fs_ext

module Atomic : AtomicSig = functor (Fs:Augmented) -> struct
	include AtomicTypes

	let readable _fs name = Lwt.return (Ok name)

	let with_writable fs dest fn =
		(* let dest = Fs.Path.to_unix dest in *)
		let success = Ok () in
		let tmpname = Path.modify_filename (fun name -> name ^ ".tmp") dest in
		let dest_unix = Path.to_unix dest in
		let tmpname_unix = Path.to_unix tmpname in
		let cleanup result = result
			|> Lwt_r.and_then (fun () -> Fs.destroy_if_exists fs tmpname_unix) in

		Fs.ensure_empty fs tmpname_unix |> Lwt_r.bind (fun () ->
			fn tmpname |> Lwt.bindr (function
				| Ok `Rollback -> cleanup success
				| Ok `Commit -> (
					let open Unix in
					(* XXX we can't get `base` out of FS_unix, because FS_unix.t is abstract.
					 * So this code relies on us calling only connect "/" *)
					try
						Unix.rename tmpname_unix dest_unix; return success
					with Unix_error (_,_,_) ->
						return (Error `No_directory_entry) (* not ideal, but we don't expect any errors *)
				)
				| Error _ as e -> cleanup e
			)
		)
end
