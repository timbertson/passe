type ('a, 'err) result = ('a, 'err) Rresult.result
module R = struct
	include Rresult.R
	let recover : ('e -> 'a) -> ('a, 'e) result -> 'a = fun fn -> function
		| Ok v -> v
		| Error e -> fn e
end

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

	let filter_map fn l =
		List.fold_right (fun item acc ->
			match fn item with
				| None -> acc
				| Some item -> item :: acc
		) l []

	let rec any pred = function
		| [] -> false
		| x::xs -> if pred x then true else any pred xs
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

module Lwt_r = struct
	let bind (type a) (type b) (type err) : (a -> (b, err) result Lwt.t) -> (a, err) result Lwt.t -> (b, err) result Lwt.t =
		fun f r -> Lwt.bind r (function
			| Ok v -> f v
			| Error e -> Lwt.return (Error e)
		)

	let map (type a)(type b)(type err): (a -> b) -> (a, err) result Lwt.t -> (b, err) result Lwt.t =
		fun f r -> Lwt.map (R.map f) r

	let bind_lwt (type a)(type b)(type err): (a -> b Lwt.t) -> (a, err) result Lwt.t -> (b, err) result Lwt.t =
		fun f r -> Lwt.bind r (function
			| Ok v -> f v |> Lwt.map R.ok
			| Error e -> Lwt.return (Error e)
		)

	let join (type a)(type err) : (a Lwt.t, err) result -> (a, err) result Lwt.t =
		function
			| Ok v -> v |> Lwt.map (fun v -> Ok v)
			| Error e -> Lwt.return (Error e)
end
