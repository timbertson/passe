open Test_common

open Store

module J = struct
	include Json_ext
	let assert_equal = assert_equal ~cmp:Json_ext.eq ~printer:to_string
end
module Db = struct
	let assert_equal = assert_equal ~cmp:Store.eq ~printer: (Store.to_json_string)
end
module Record = struct
	let assert_equal = assert_equal
		~cmp:(Store.record_eq)
		~printer:(Store.json_string_of_record)
end
module DomainFieldChange = struct
	open Store
	let parse = Format.domain_field_change_of_json
	let assert_equal = assert_equal ~printer:(fun ch -> Format.json_of_field_change ch |> J.to_string)
end

let default_domain = Store.default (Store.empty)
let suite = "db" >:::
	let entry = { default_domain "example.com" with note = Some "note"; } in
	[
	"parsing changes" >::: [
		"dropping an optional value" >:: (fun _ctx ->
			let db = { empty with core = { empty_core with
				records = StringMap.empty |> StringMap.add "example.com" (Domain entry)
			}} in
			let updated = update ~db
				~original:(Some (Domain entry))
				(Some (Domain ({entry with note = None; }))) in

			let json = updated |> to_json in
			(* log#debug "serialized: %s" (J.to_string json); *)
			json |> parse_json |> Db.assert_equal updated
		);
	];

	"record parsing" >::: [
		"fails on unknown fields when parsing a record" >:: (fun _ctx ->
			let json = ("example.com", `Assoc [
				"length", `Int 10;
				"type", `String "domain";
				"hint", `String "note";
				"unknown_key", `String "...";
			]) in
			assert_raises
				(Common.AssertionError "Un-processed keys after parsing: unknown_key")
				(fun () -> Format.parse_record json)
		);
	];

	"hint -> note upgrade" >:::
		let old_key = "hint" in
		let new_key = "note" in
		let json_change key v = `List [`String key; `String v] in
		[
		"understands both old and new-style note changes" >:: (fun _ctx ->
			let open DomainFieldChange in
			parse (json_change old_key "n") |> assert_equal (`Note (Some "n"));
			parse (json_change new_key "n") |> assert_equal (`Note (Some "n"));
		);

		"generates old-style note" >:: (fun _ctx ->
			(* we can't generate new-style hints until all clients support it *)
			Format.json_of_field_change (`Note (Some "n")) |> J.assert_equal (json_change old_key "n")
		);

		"saves note in old field" >:: (fun _ctx ->
			(* parsing ignores unknown fields, so we can just save both for future compat *)
			Format.json_of_record (Domain entry) |> J.assert_equal (`Assoc [
				("example.com", `Assoc [
					"length", `Int 10;
					"type", `String "domain";
					old_key, `String "note";
				])
			])
		);
	]
]
