open Passe
open Common
open Filesystem

(* pure abstraction over the state of an "atomic" file *)
module Journal = struct
	type ext = [ `primary | `secondary ]

	let _tmpname = function
		| `primary -> `secondary
		| `secondary -> `primary

	let journal_name name = name ^ ".j"
	let cruft ext = _tmpname ext
	
	let dereference ~try_read_char name =
		let journal_name = journal_name name in
		try_read_char journal_name |> Lwt.map @@ R.bindr (function
			| Some '2' -> Ok `secondary
			| Some '1' | None -> Ok `primary
			| _other -> Error `No_directory_entry
		)
	
	let byte = function `primary -> '1' | `secondary -> '2'

	let apply_ext ext name =
		match ext with
			| `primary -> name
			| `secondary -> name ^ ".2"
	
	let writeable_ext ~try_read_char name =
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
		Journal.dereference ~try_read_char:(try_read_char fs) (Path.to_unix name)
			|> Lwt_r.map (fun ext -> Path.modify_filename (Journal.apply_ext ext) name)

	let with_writable fs dest fn =
		let dest_s = Path.to_unix dest in
		let journal = Journal.journal_name dest_s in
		Journal.writeable_ext ~try_read_char:(try_read_char fs) dest_s
			|> Lwt.map (R.reword_error as_write_error)
			|> Lwt_r.bind (fun dest_ext ->
				let newdest = Path.modify_filename (Journal.apply_ext dest_ext) dest in
				let rollback result =
					result |> Lwt_r.and_then (fun () ->
						destroy_if_exists fs newdest
					)
				in
				let newdest_s = Path.to_unix newdest in
				let cruft_path = Path.modify_filename (Journal.apply_ext (Journal.cruft dest_ext)) dest in
				ensure_empty fs newdest_s |> Lwt_r.bind (fun () ->
					fn newdest |> Lwt.bindr (function
						| Ok `Commit -> (
							let open Lwt_r.Infix in
							let journal_byte = Cstruct.create 1 in
							Cstruct.set_char journal_byte 0 (Journal.byte dest_ext);
							ensure_exists fs journal >>= fun () ->
							write fs journal 0 journal_byte >>= fun () ->
							destroy_if_exists fs cruft_path
						)
						| Ok `Rollback -> rollback (Ok ())
						| Error _ as result -> rollback result
					)
				)
			)
end
