open Test_common
open OUnit2
open Passe.Common.List

open I.L

let suite = "List" >::: [
	"take" >::: [
		"on empty list" >:: (fun _ -> take 5 [] |> assert_equal []);
		"0 items" >:: (fun _ -> take 0 [1;2;3] |> assert_equal []);
		"more than list length" >:: (fun _ -> take 5 [1;2] |> assert_equal [1;2]);
		"less than list length" >:: (fun _ -> take 2 [1;2;3;4;5] |> assert_equal [1;2]);
	]
]

