open Passe
open Lwt
open Common
open Filesystem

module Atomic : AtomicSig = functor (Fs:Fs_ext.Impl) -> struct
	let readable _fs name = Lwt.return (Ok (Fs.Path.to_unix name))

	let with_writable fs dest fn =
		let dest = Fs.Path.to_unix dest in
		let success = Ok () in
		let tmpname = dest ^ ".tmp" in
		let cleanup result = result
			|> Lwt_r.and_then (fun () -> Fs.destroy_if_exists_s fs tmpname) in

		Fs.ensure_empty fs tmpname |> Lwt_r.bind (fun () ->
			fn tmpname |> Lwt.bindr (function
				| Ok `Rollback -> cleanup success
				| Ok `Commit -> (
					let open Unix in
					(* XXX we can't get `base` out of FS_unix, because FS_unix.t is abstract.
					 * So this code relies on us calling only connect "/" *)
					try
						Unix.rename tmpname dest; return success
					with Unix_error (_,_,_) ->
						return (Error `No_directory_entry) (* not ideal, but we don't expect any errors *)
				)
				| Error _ as e -> cleanup e
			)
		)
end