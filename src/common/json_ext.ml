include Yojson.Safe

let as_string obj = match obj with
	| `String s -> Some s | _ -> None

let get_field key obj = match obj with
	| `Assoc pairs -> (
			match Common.find_safe (fun (k,v) -> k = key) pairs with
				| Some (_key, v) -> Some v
				| _ -> None
		)
	| _ -> None

let without_field key obj = match obj with
	| Some (`Assoc pairs) -> (
			let pairs = (List.filter (fun (k,v) -> k <> key) pairs) in
			match pairs with
				| [] -> None
				| _ -> Some (`Assoc pairs)
		)
	| None -> None
	| _ -> raise (Common.AssertionError "can't remove field on non-object")

let set_field key value obj = match (without_field key obj) with
	| Some (`Assoc pairs) -> `Assoc ((key, value) :: pairs)
	| None -> `Assoc [(key, value)]
	| _ -> raise (Common.AssertionError "can't set field on non-object")
