open Passe
open Lwt
open Common
open Filesystem

module Atomic : AtomicSig = functor (Common:FSCommonSig) -> struct
	type write_commit = Common.write_commit
	let readable _fs name = Lwt.return (Ok name)

	let with_writable fs dest fn =
		let success = Ok () in
		let tmpname = dest ^ ".tmp" in
		let cleanup result = result
			|> Lwt_r.and_then (fun () -> Common.destroy_if_exists fs tmpname) in

		Common.ensure_empty fs tmpname |> Lwt_r.bind (fun () ->
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
