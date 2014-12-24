open Test_common
open OUnit2

module Version = struct
	include Version
	let assert_equal = assert_equal ~printer:(fun (a,b,c) ->
		"("^ (
			[a;b;c] |> List.map string_of_int |> String.concat ","
		)^")")
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
