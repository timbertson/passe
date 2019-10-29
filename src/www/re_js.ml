open Js_of_ocaml
open Passe
type t = Regexp.regexp
let split = Regexp.split
let regexp = Regexp.regexp
let regexp_string = Regexp.regexp_string
let replace_first r by s = Regexp.replace_first r s by
let string_match r s = Regexp.string_match r s 0 |> Option.is_some
let contains r s = Regexp.search_forward r s 0 |> Option.is_some
