include Yojson.Safe
let to_string = pretty_to_string ~std:true
let print_chan = Yojson.Safe.pretty_to_channel ~std:true
let print_str () = to_string
let print = print_str
let fmt formatter obj = Format.pp_print_string formatter (to_string obj)

let to_single_line_string j =
	let rv = Yojson.Safe.to_string j in
	if String.contains rv '\n' then failwith "to_json result contains a newline";
	rv

let print_oneline () = to_single_line_string

let as_string obj = match obj with
	| `String s -> Some s | _ -> None

let as_int obj = match obj with
	| `Int i -> Some i | _ -> None

let as_list obj = match obj with
	| `List i -> Some i | _ -> None

let as_bool obj = match obj with
	| `Bool i -> Some i | _ -> None

let get_field key obj = match obj with
	| `Assoc pairs -> (
			match Common.find_safe (fun (k,_v) -> k = key) pairs with
				| Some (_key, v) -> Some v
				| _ -> None
		)
	| _ -> None

let mandatory getter key obj = match (getter key obj) with
	| Some x -> x
	| None -> failwith ("missing JSON field: " ^ key)

let get_field_as fn key j = get_field key j |> Option.bind fn
let string_field : string -> json -> string option = get_field_as as_string
let int_field : string -> json -> int option = get_field_as as_int
let list_field : string -> json -> json list option = get_field_as as_list
let bool_field : string -> json -> bool option = get_field_as as_bool

type obj = [
	| `Assoc of (string * json) list
]

let as_object = function
	| `Assoc _ as obj -> obj
	| j -> Error.raise_assert ("Expected JSON object, got " ^ (to_string j))

let without_field key (obj:obj option) = match obj with
	| Some (`Assoc pairs) -> (
			let pairs = (List.filter (fun (k,_v) -> k <> key) pairs) in
			match pairs with
				| [] -> None
				| _ -> Some (`Assoc pairs)
		)
	| None -> None

let set_field key value (obj:obj option) = match (without_field key obj) with
	| Some (`Assoc pairs) -> `Assoc ((key, value) :: pairs)
	| None -> `Assoc [(key, value)]

let rec eq a b =
	let rec list_eq eq a b = match (a, b) with
		| [], [] -> true
		| a::aa, b::bb -> eq a b && list_eq eq aa bb
		| [], _ -> false
		| _, [] -> false
	in
	match a, b with
	| `Assoc a, `Assoc b ->
			let cmp = (fun (key_a, _) (key_b, _) -> compare key_a key_b) in
			let a = List.sort cmp a
			and b = List.sort cmp b in
			list_eq (fun (key_a, val_a) (key_b, val_b) -> key_a = key_b && eq val_a val_b) a b
	| `Assoc _, _ -> false

	| `Bool a, `Bool b -> a = b
	| `Bool _, _ -> false

	| `Float a, `Float b -> a = b
	| `Float _, _ -> false

	| `Int a, `Int b -> a = b
	| `Int _, _ -> false

	| `Intlit a, `Intlit b -> a = b
	| `Intlit _, _ -> false

	| `List a, `List b ->
			let rec list_eq a b = match (a, b) with
				| [], [] -> true
				| a::aa, b::bb -> eq a b && list_eq aa bb
				| [], _ -> false
				| _, [] -> false
			in
			list_eq a b
	| `List _, _ -> false

	| `Null, `Null -> true
	| `Null, _ -> false

	| `String a, `String b -> a = b
	| `String _, _ -> false

	| `Tuple _, _ -> failwith "tuples not yet supported"
	| `Variant _, _ -> failwith "variants not yet supported"

let typeof = function
	| `Assoc _ -> "Object"
	| `Bool _ -> "Boolean"
	| `Float _ -> "Float"
	| `Int _ -> "Int"
	| `Intlit _ -> "Intlit"
	| `List _ -> "List"
	| `Null -> "Null"
	| `String _ -> "String"
	| `Tuple _ -> "Tuple"
	| `Variant _ -> "Variant"

let empty = `Assoc []
