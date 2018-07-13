open Test_common
open OUnit2

(* TODO: test JS version somehow *)
open Passe_unix.Re

let assert_matches re s = B.assert_equal true (string_match re s)
let assert_not_matches re s = B.assert_equal false (string_match re s)

let suite = "re" >::: [
	"regxep_string" >:: (fun _ ->
		assert_matches (regexp_string "*") "*...";
		assert_not_matches (regexp_string ".") "-";
	);

	"replace_first" >:: (fun _ ->
		S.assert_equal "ba" (replace_first (regexp "a.") "b" "aaa");
	);

	"contains" >:: (fun _ ->
		B.assert_equal true (contains (regexp "oo") "foo");
		B.assert_equal false (contains (regexp "oo") "fo");
	);

	"split" >:: (fun _ ->
		S.L.assert_equal ["a"; "bcd"; "e"] (split (regexp " +") "a   bcd e")
	);
]
