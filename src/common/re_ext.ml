module type Sig = sig
	type t
	val split : t -> string -> string list
	val regexp : string -> t
	val quote : string -> string
	val regexp_string : string -> t
	val string_match : t -> string -> bool
	val contains : t -> string -> bool
	val replace_first : t -> string -> string -> string
end
