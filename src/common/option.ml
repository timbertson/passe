type 'a t = 'a option
let return x = Some x

let map f opt = match opt with Some x -> Some (f x) | None -> None
let may f opt = match opt with
	| Some x -> (f x)
	| None -> ()

let default_fn f opt = match opt with
	| Some x -> x
	| None -> (f ())

let default (d:'a) (opt:'a option) : 'a = match opt with Some x -> x | None -> d

let print_chan sub_printer chan v = match v with
	| Some x -> output_string chan "Some("; sub_printer chan x; output_char chan ')'
	| None -> output_string chan "None()"

let print_str sub_printer () v = match v with
	| Some x -> "Some(" ^ (sub_printer () x) ^ ")"
	| None -> "None"

let print = print_str
let to_string sub = print_str (fun () -> sub) ()
let to_list = function Some x -> [x] | None -> []
let fmt sub formatter = function
	| Some v ->
		Format.pp_print_string formatter "Some(";
		sub formatter v;
		Format.pp_print_string formatter ")"
	| None ->
		Format.pp_print_string formatter "None"

let bind fn opt = match opt with
	| Some v -> fn v
	| None -> None

let filter fn = function
	| Some x as rv when fn x -> rv
	| Some _ | None -> None

let or_fn fn opt = match opt with
	| Some _ as v -> v
	| None -> fn ()

let or_ alt opt = match opt with
	| Some _ as v -> v
	| None -> alt

let some x = Some x

let is_some = function Some _ -> true | None -> false
let is_none = function Some _ -> false | None -> true
let get default = function Some x -> x | None -> default
let get_exn v exn = match v with Some x -> x | None -> raise exn
let force = function Some x -> x | None -> raise Not_found

let non_empty ~zero v = if v = zero then None else Some v

let cond test value = if test then Some value else None

let eq eq a b = match (a,b) with
	| Some a, Some b -> eq a b
	| Some _, None -> false
	| None, Some _ -> false
	| None, None -> true
