let map f opt = match opt with Some x -> Some (f x) | None -> None
let may f opt = match opt with
	| Some x -> (f x); ()
	| None -> ()

let default (d:'a) (opt:'a option) : 'a = match opt with Some x -> x | None -> d
let print_minimal sub_printer chan opt v = match v with
	| Some x -> sub_printer chan x
	| None -> ()

let print sub_printer chan v = match v with
	| Some x -> output_string chan "Some("; sub_printer chan x; output_char chan ')'
	| None -> output_string chan "None()"

let print_str sub_printer () v = match v with
	| Some x -> "Some(" ^ (sub_printer () x) ^ ")"
	| None -> "None()"

let is_some = function Some _ -> true | None -> false
let is_none = function Some _ -> false | None -> true
let get default = function Some x -> x | None -> default
