open Filesystem
module Atomic : AtomicSig = functor (Common:FSCommonSig) -> struct
	type write_commit = Common.write_commit
	let readable fs name = Lwt.return name


	let with_writable fs dest fn =
		let tmpname = dest ^ ".tmp" in
		let cleanup () = Common.unwrap_lwt "destroy" (Common.destroy_if_exists fs tmpname) in
		lwt () = Common.ensure_empty fs tmpname in
		try_lwt
			lwt result = fn tmpname in
			match result with
				| `Rollback -> cleanup ()
				| `Commit ->
					(* XXX we can't get `base` out of FS_unix, because FS_unix.t is abstract.
					 * So this code relies on us calling only connect "/" *)
					Unix.rename tmpname dest;
					Lwt.return_unit
		with e -> begin
			lwt () = cleanup () in
			raise e
		end
end
