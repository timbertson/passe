open Passe
open Common
open Lwt
open Result

type stat = Mirage_fs.stat

module type FS = sig
	include Mirage_fs_lwt.S
		with type +'a io = 'a Lwt.t
end

module type Sig = sig
	type t
	type error = private [> `Is_a_directory | `No_directory_entry | `Not_a_directory ]
	type write_error = private [>
		| `Is_a_directory
		| `No_directory_entry
		| `Not_a_directory
		| `File_already_exists
		| `No_space
	]

	type write_instruction = [ `Output of string | `Rollback ]

	type write_commit = [ `Commit | `Rollback ]

	val string_of_error : error -> string

	val string_of_write_error : write_error -> string

	val as_write_error : error -> write_error

	val destroy_if_exists : t -> string -> (unit, write_error) result Lwt.t

	val ensure_empty : t -> string -> (unit, write_error) result Lwt.t

	val ensure_exists : t -> string -> (unit, write_error) result Lwt.t

	val stat : t -> string -> (stat, error) result Lwt.t

	val mkdir : t -> string -> (unit, write_error) result Lwt.t
end

module type Impl = sig
	include FS
	include Sig
		with type t := t
		and type error := error
		and type write_error := write_error
end


module Make (Fs:FS) : (Impl with type t = Fs.t) = struct
	include Fs

	type write_instruction = [ `Output of string | `Rollback ]
	type write_commit = [ `Commit | `Rollback ]
	let _error_message pp e = Format.asprintf "%a" pp e
	let string_of_error = _error_message pp_error
	let string_of_write_error = _error_message pp_write_error

	(* this is inelegant... *)
	let as_write_error (err:error) : write_error = match err with
		(* should be able to just upcast this... *)
		| `Is_a_directory -> `Is_a_directory
		| `No_directory_entry -> `No_directory_entry
		| `Not_a_directory -> `Not_a_directory

	(* returns whether the file was created *)
	let _ensure_exists fs path =
		stat fs path >>= (function
			| Ok _ -> return (Ok false)
			| Error `No_directory_entry ->
				create fs path |> Lwt_r.map (fun () -> true)
			| Error err -> return (Error (err |> as_write_error))
		)

	let ensure_exists fs path =
		_ensure_exists fs path |> Lwt_r.map (fun (_created:bool) -> ())

	let ensure_empty fs path =
		(* Note: not atomic *)
		_ensure_exists fs path |> Lwt_r.bind (fun created ->
			if not created then begin
				(* recreate to truncate *)
				destroy fs path |> Lwt_r.bind (fun () ->
					create fs path
				)
			end else return (Ok ())
		)
	
	let destroy_if_exists fs path =
		match_lwt destroy fs path with
			| Ok _ | Error `No_directory_entry -> return (Ok ())
			| Error _ as e -> return e
end
