open Test_common
open OUnit2

module Version = struct
	include Version
	include MakeAssert(struct
		type t = int * int * int
		let cmp = None
		let printer (a,b,c) = I.L.printer ~brackets:("(",")") [a;b;c]
	end)
end

open Version

let suite = "version" >::: [
	"parses" >::: [
		"plain" >:: (fun _ ->
			parse "10.2.3" |> assert_equal (10, 2, 3)
		);
		"development" >:: (fun _ ->
			parse "10.2.3-1234abcd" |> assert_equal (10, 2, 3)
		);
	]
]
