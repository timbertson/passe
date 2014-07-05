include Yojson.Safe
let print = Yojson.Safe.pretty_to_channel ~std:false
let print_str () = pretty_to_string ~std:false

let as_string obj = match obj with
	| `String s -> Some s | _ -> None

let get_field key obj = match obj with
	| `Assoc pairs -> (
			match Common.find_safe (fun (k,v) -> k = key) pairs with
				| Some (_key, v) -> Some v
				| _ -> None
		)
	| _ -> None

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
	| _ -> raise (Common.AssertionError "can't remove field on non-object")

let set_field key value (obj:obj option) = match (without_field key obj) with
	| Some (`Assoc pairs) -> `Assoc ((key, value) :: pairs)
	| None -> `Assoc [(key, value)]
	| _ -> raise (Common.AssertionError "can't set field on non-object")
