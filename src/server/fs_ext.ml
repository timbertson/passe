open Passe
open Common
open Lwt
open Result
open Astring

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

	module Path : sig
		type base
		type t
		type invalid_path = [ `Invalid_path ]

		val pp : t Fmt.t
		val pp_full : t Fmt.t
		val base : string -> base
		val make : base -> string list -> (t, invalid_path) result
		val modify_filename : (string -> string) -> t -> t
		val to_unix : t -> string
		val string_of_invalid_path : invalid_path -> string
	end

	module PathMap : Map.S with type key = Path.t

	type write_instruction = [ `Output of string | `Rollback ]

	type write_commit = [ `Commit | `Rollback ]

	val string_of_error : error -> string

	val string_of_write_error : write_error -> string

	val as_write_error : error -> write_error

	val stat : t -> Path.t -> (stat, error) result Lwt.t

	val mkdir : t -> Path.t -> (unit, write_error) result Lwt.t

	val destroy_if_exists : t -> Path.t -> (unit, write_error) result Lwt.t
end


module type Impl = sig
	include FS
	include Sig
		with type t := t
		and type error := error
		and type write_error := write_error
	val ensure_empty : t -> string -> (unit, write_error) result Lwt.t
	val ensure_exists : t -> string -> (unit, write_error) result Lwt.t
	val destroy_if_exists_s : t -> string -> (unit, write_error) result Lwt.t
end


module Make (Fs:FS) : (Impl with type t = Fs.t) = struct
	include Fs

	module Path = struct
		type base = string

		(* base is an arbitrary string.
		 * parts is guaranteed to be a nonempty sequence of filenames
		 * - i.e. no part contains slashes or leading dots *)
		type t = string * (string list)
		type relative = string list
		type invalid_path = [ `Invalid_path ]
		let string_of_invalid_path = function `Invalid_path -> "Invalid_path"
		let base path =
			if (Filename.is_relative path)
				then failwith ("relative path used for Path.base:" ^ (path))
				else path

		let invalid_component = function
			| "" -> true
			| part -> String.is_prefix ~affix:"." part || String.is_infix ~affix:"/" part

		(* TODO: this would be more efficient if we stored `t` as (string * string list * string) *)
		let modify_filename modifier (base, parts) =
			match (List.rev parts) with
				| f :: tail ->
					let modified = modifier f in
					if invalid_component modified
						then raise (AssertionError "`Invalid_path")
						else (base, (List.rev tail) @ [modified])
				| [] ->
					(* not possible to construct, just for type-completeness *)
					raise (AssertionError "`Invalid_path")

		let make base parts =
			if (parts = []) || (parts |> List.any invalid_component)
				then Error `Invalid_path
				else Ok (base, parts)

		let _pp ~full formatter (base, path) =
			let fmt_slash = Fmt.const Fmt.string Filename.dir_sep in
			let parts = if full then [base] @ path else path in
			(Fmt.list ~sep:fmt_slash Fmt.string) formatter parts

		let pp = _pp ~full:false
		let pp_full = _pp ~full:true

		let to_unix (base, path) =
			(String.concat ~sep:Filename.dir_sep (base :: path))
	end

	module PathMap = Map.Make (struct
		type t = Path.t
		let compare (abase,a) (bbase,b) =
			let compare_one : string -> string -> int = Pervasives.compare in
			let rec compare_parts a b = (
				match (a,b) with
					| [], [] -> 0
					| [], _ -> -1
					| _, [] -> 1
					| (a1::a, b1::b) -> (match compare_one a1 b1 with
						| 0 -> compare_parts a b
						| diff -> diff
					)
			) in
			compare_parts (abase::a) (bbase::b)
	end)

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
	
	let destroy_if_exists_s fs path =
		match%lwt destroy fs path with
			| Ok _ | Error `No_directory_entry -> return (Ok ())
			| Error _ as e -> return e

	let destroy_if_exists fs = destroy_if_exists_s fs % Path.to_unix
	let stat fs = stat fs % Path.to_unix
	let mkdir fs = mkdir fs % Path.to_unix

end
