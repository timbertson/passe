module Str = Re_str
type t = Str.regexp
let regexp = Str.regexp
let regexp_string = Str.regexp_string
let split = Str.split
let replace_first = Str.replace_first
let string_match r s = Str.string_match r s 0
let contains r s = try let (_:int) = Str.search_forward r s 0 in true with Not_found -> false
