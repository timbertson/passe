exception AssertionError of string
type ('a, 'b) either = Left of 'a | Right of 'b

module StringMap = struct
	include Map.Make(String)
	let from_pairs pairs = List.fold_left (fun map (k,v) -> add k v map) empty pairs
end

let find_safe fn l = try Some (List.find fn l) with Not_found -> None

let (%) f g = (fun x -> f (g x))

let finally_do cleanup resource f =
	let result =
		try f resource
		with ex -> cleanup resource; raise ex in
	let () = cleanup resource in
	result

let print_string () s = s
