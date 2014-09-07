include Yojson.Safe
let print_chan = Yojson.Safe.pretty_to_channel ~std:false
let print_str () = pretty_to_string ~std:false
let print = print_str

let as_string obj = match obj with
	| `String s -> Some s | _ -> None

let as_int obj = match obj with
	| `Int i -> Some i | _ -> None

let as_float obj = match obj with
	| `Float f -> Some f | _ -> None

let as_list obj = match obj with
	| `List i -> Some i | _ -> None

let as_bool obj = match obj with
	| `Bool i -> Some i | _ -> None

let get_field key obj = match obj with
	| `Assoc pairs -> (
			match Common.find_safe (fun (k,v) -> k = key) pairs with
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
let float_field : string -> json -> float option = get_field_as as_float
let bool_field : string -> json -> bool option = get_field_as as_bool

type obj = [
	| `Assoc of (string * json) list
]

let as_object = function
	| `Assoc _ as obj -> obj
	| j -> raise @@ Common.AssertionError ("Expected JSON object, got " ^ (pretty_to_string ~std:true j))

let without_field key (obj:obj option) = match obj with
	| Some (`Assoc pairs) -> (
			let pairs = (List.filter (fun (k,v) -> k <> key) pairs) in
			match pairs with
				| [] -> None
				| _ -> Some (`Assoc pairs)
		)
	| None -> None

let set_field key value (obj:obj option) = match (without_field key obj) with
	| Some (`Assoc pairs) -> `Assoc ((key, value) :: pairs)
	| None -> `Assoc [(key, value)]

let to_single_line_string ~std j =
	let rv = Yojson.Safe.to_string ~std j in
	if String.contains rv '\n' then failwith "to_json result contains a newline";
	rv

