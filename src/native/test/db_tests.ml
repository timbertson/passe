open Batteries
open Passe
open OUnit2
module J = Json_ext

module Db = struct
	let assert_equal = assert_equal ~cmp:Store.eq ~printer: (Store.to_json_string)
end

let default_domain = Store.default (Store.empty)
let suite = "db" >:::
	let open Store in
	[
	"parsing changes" >::: [
		"dropping an optional value" >:: (fun ctx ->
			let entry = { default_domain "example.com" with
				note = Some "note";
			} in
			let db = { empty with core = { empty_core with
				records = StringMap.empty |> StringMap.add "example.com" (Domain entry)
			}} in
			let updated = update ~db
				~original:(Some (Domain entry))
				(Some (Domain ({entry with note = None; }))) in
			let json = updated |> to_json in
			log#debug "serialized: %s" (J.to_string json);
			json |> parse_json |> Db.assert_equal updated
		);
	]
]
