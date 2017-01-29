include List
let filter_map fn l =
	List.fold_right (fun item acc ->
		match fn item with
			| None -> acc
			| Some item -> item :: acc
	) l []

let rec any pred = function
	| [] -> false
	| x::xs -> if pred x then true else any pred xs

