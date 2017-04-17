type ('a, 'err) result = ('a, 'err) Rresult.result
module R = Rresult.R

exception SafeError of string
exception AssertionError of string
type ('a, 'b) either = Left of 'a | Right of 'b
module Either = struct
	let to_option = function
		| Left _ -> None
		| Right x -> Some x
end

module StringMap = struct
	include Map.Make(String)
	let from_pairs pairs = List.fold_left (fun map (k,v) -> add k v map) empty pairs
end

module List = struct
	include List
	let take n l =
		let rec _take rev_results n l =
			if n <= 0 then List.rev rev_results else match l with
				| [] -> List.rev rev_results
				| head::tail -> _take (head::rev_results) (n-1) tail
		in
		_take [] n l
end

let startswith big sml = String.sub big 0 (String.length sml) = sml

let find_safe fn l = try Some (List.find fn l) with Not_found -> None

let (%) f g = (fun x -> f (g x))

let finally_do cleanup resource f =
	let result =
		try f resource
		with ex -> cleanup resource; raise ex in
	let () = cleanup resource in
	result

let print_string () s = s

let identity x = x

let quote_string s = "\"" ^ String.escaped s ^ "\""

let mask_string s = String.make (String.length s) '*'
