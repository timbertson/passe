exception AssertionError of string
type ('a, 'b) either = Left of 'a | Right of 'b

module StringMap = struct
	include Map.Make(String)
	let from_pairs pairs = List.fold_left (fun map (k,v) -> add k v map) empty pairs
end
