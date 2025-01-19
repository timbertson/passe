open Passe
open Common
open Astring

module Path : sig
	type base
	type relative
	type full

	val pp : relative Fmt.t
	val pp_full : full Fmt.t
	val base : string -> base

	val make : string list -> (relative, [> Error.invalid]) result
	val modify_filename : (string -> string) -> full -> full
	val join : base -> relative -> full
	val to_unix : full -> string
	val to_string : relative -> string

	val ext : relative -> string option

	module Full : OrderedType.S with type t = full
	module Relative : OrderedType.S with type t = relative
end = struct
	(* base is an absolute unix path *)
	type base = string

	let base path =
		if (Filename.is_relative path)
			then Error.failwith (`Invalid ("relative path used for Path.base:" ^ (path)))
			else path

	let ext path =
		String.cut ~rev:true ~sep:"." (snd path) |> Option.map snd

	(* relative is guaranteed to be a nonempty sequence of filenames
	 * - i.e. no part contains slashes or leading dots *)
	type relative = (string list) * string

	type full = base * relative

	let invalid_component = function
		| "" -> true
		| part -> String.is_prefix ~affix:"." part || String.is_infix ~affix:"/" part

	let modify_filename modifier (base, (parts, fname)) =
		let modified = modifier fname in
		if invalid_component modified
			then Error.failwith (`Invalid "path")
			else (base, (parts, modified))

	let make parts =
		if (parts |> List.any invalid_component)
			then Error (`Invalid "path component")
			else match List.rev parts with
				| fname :: path -> Ok (List.rev path, fname)
				| [] -> Error (`Invalid "path component")

	let pp formatter (parts, fname) =
		let fmt_slash = Fmt.const Fmt.string Filename.dir_sep in
		(Fmt.list ~sep:fmt_slash Fmt.string) formatter (parts @ [fname])

	let pp_full formatter (base, (parts, fname)) = pp formatter ((base :: parts), fname)

	let to_string path = pp_strf pp path

	let to_unix (base, (path, fname)) = to_string (base :: path, fname)

	let join base rel = (base, rel)

	module Relative = struct
		type t = relative
		let compare_one : string -> string -> int = Pervasives.compare
		let compare (a, af) (b, bf) = (
			let rec compare a b = (
				match (a,b) with
					| [], [] -> 0
					| [], _ -> -1
					| _, [] -> 1
					| (a1::a, b1::b) -> (match compare_one a1 b1 with
						| 0 -> compare a b
						| diff -> diff
					)
			) in

			match compare_one af bf with
			| 0 -> compare a b
			| diff -> diff
		)
		let pp = pp
	end

	module Full = struct
		type t = full
		let compare (abase,(a,af)) (bbase,(b,bf)) = Relative.compare (abase::a, af) (bbase::b, bf)
		let pp = pp_full
	end

end

include Path
