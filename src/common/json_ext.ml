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
