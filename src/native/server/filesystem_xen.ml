open Filesystem

(* pure abstraction over the state of an "atomic" file *)
module Journal = struct
	type ext = [ `primary | `secondary ]
	type name = string * ext option

	let _tmpname (name, ext) =
		(name, match ext with
			| `primary -> `secondary
			| `secondary -> `primary)

	let journal_name name = name ^ ".j"
	let cruft name = _tmpname name
	
	let dereference ~try_read_char name =
		let journal_name = journal_name name in
		lwt ch = try_read_char journal_name in
		Lwt.return (match ch with
			| Some '2' -> (name, `secondary)
			| Some '1' | None -> (name, `primary)
			| other -> failwith "Unexpected character in journal")
	
	let byte (name, ext) = match ext with `primary -> '1' | `secondary -> '2'

	let string_of_name (name, ext) =
		match ext with
			| `primary -> name
			| `secondary -> name ^ ".2"
	
	let writeable_name ~try_read_char name =
		lwt deref = dereference ~try_read_char name in
		Lwt.return (_tmpname deref)

end

module Atomic : AtomicSig = functor (Common:FSCommonSig) -> struct
	type write_commit = Common.write_commit
	let try_read_char fs name =
		lwt result = Common.read fs name 0 1 in
		match result with
			| `Ok chunks ->
					let chunk = chunks |> List.filter (fun chunk -> Cstruct.len chunk > 0) |> List.hd in
					Lwt.return (Some (Cstruct.get_char chunk 0))
			| `Error `No_directory_entry (_,_) -> Lwt.return_none
			| `Error e -> Common.fail "read" e

	let readable fs name =
		lwt name = Journal.dereference ~try_read_char:(try_read_char fs) name in
		Lwt.return (Journal.string_of_name name)

	let with_writable fs dest fn =
		let journal = Journal.journal_name dest in
		lwt newdest = Journal.writeable_name ~try_read_char:(try_read_char fs) dest in
		let newdest_s = newdest |> Journal.string_of_name in
		lwt () = Common.ensure_empty fs newdest_s in
		let cleanup () = Common.destroy_if_exists fs newdest_s |> Common.unwrap_lwt "destroy" in
		lwt result = try_lwt
				fn newdest_s
			with e -> begin
				lwt () = cleanup () in
				raise e
			end
		in
		match result with
			| `Rollback -> cleanup ()
			| `Commit ->
				let journal_byte = Cstruct.create 1 in
				Cstruct.set_char journal_byte 0 (Journal.byte newdest);
				lwt () = Common.ensure_exists fs journal in
				lwt () = Common.write fs journal 0 journal_byte |> Common.unwrap_lwt "write" in
				lwt () = Common.destroy_if_exists fs (Journal.cruft newdest |> Journal.string_of_name) |> Common.unwrap_lwt "destroy" in
				Lwt.return_unit
end
