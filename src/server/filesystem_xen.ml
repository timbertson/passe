open Passe
open Common
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
		try_read_char journal_name |> Lwt.map @@ R.bindr (function
			| Some '2' -> Ok (name, `secondary)
			| Some '1' | None -> Ok (name, `primary)
			| _other -> Error `No_directory_entry
		)
	
	let byte (_name, ext) = match ext with `primary -> '1' | `secondary -> '2'

	let string_of_name (name, ext) =
		match ext with
			| `primary -> name
			| `secondary -> name ^ ".2"
	
	let writeable_name ~try_read_char name =
		dereference ~try_read_char name |> Lwt_r.map _tmpname

end

module Atomic : AtomicSig = functor (Fs:Fs_ext.Impl) -> struct
	include Fs

	let try_read_char fs name =
		read fs name 0 1 |> Lwt.map (function
			| Ok chunks ->
					let chunk = chunks |> List.filter (fun chunk -> Cstruct.len chunk > 0) |> List.hd in
					Ok (Some (Cstruct.get_char chunk 0))
			| Error `No_directory_entry -> Ok None
			| Error _ as e -> e
		)

	let readable fs name =
		let name = Path.to_unix name in
		Journal.dereference ~try_read_char:(try_read_char fs) name
			|> Lwt_r.map Journal.string_of_name

	let with_writable fs dest fn =
		let dest = Path.to_unix dest in
		let journal = Journal.journal_name dest in
		Journal.writeable_name ~try_read_char:(try_read_char fs) dest
			|> Lwt.map (R.reword_error as_write_error)
			|> Lwt_r.bind (fun newdest ->
				let newdest_s = Journal.string_of_name newdest in
				let rollback result =
					result |> Lwt_r.and_then (fun () ->
						destroy_if_exists_s fs newdest_s
					)
				in
				ensure_empty fs newdest_s |> Lwt_r.bind (fun () ->
					fn newdest_s |> Lwt.bindr (function
						| Ok `Commit -> (
							let open Lwt_r.Infix in
							let journal_byte = Cstruct.create 1 in
							Cstruct.set_char journal_byte 0 (Journal.byte newdest);
							ensure_exists fs journal >>= fun () ->
							write fs journal 0 journal_byte >>= fun () ->
							destroy_if_exists_s fs (Journal.cruft newdest |> Journal.string_of_name)
						)
						| Ok `Rollback -> rollback (Ok ())
						| Error _ as result -> rollback result
					)
				)
			)
end
