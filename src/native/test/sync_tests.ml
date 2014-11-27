open Batteries
open Passe
open OUnit2
module J = Json_ext

let post_json ?token ~data url = Server.post_json ?token ~data url |> Lwt_main.run
let get_json ?token url = Server.get_json ?token url |> Lwt_main.run
let log = Logging.get_logger "sync_tests"

let identity = fun x -> x
let str_eq (a:string) (b:string) = a = b
let int_eq (a:int) (b:int) = a = b

module Response = struct
	let string_of_response ?printer = let open Server in function
		| OK x -> "OK: " ^ (match printer with Some s -> s x | None -> "[no printer]")
		| Unauthorized x -> "Unauthorized: " ^ (Option.to_string identity x)
		| Failed (code, x, json) -> "Failed["^(string_of_int code)^"]: " ^ x ^ (
			json |> Option.map J.to_string |> Option.map (fun j -> " ["^j^"]") |> Option.default ""
		)

	let assert_equal ~cmp ~printer = assert_equal ~cmp:(fun a b ->
		let open Server in match a, b with
		| OK a, OK b -> cmp a b
		| Unauthorized a, Unauthorized b -> Option.eq str_eq a b
		| Failed (ca,sa,ja), Failed (cb,sb,jb) -> int_eq ca cb && str_eq sa sb && (Option.eq Json_ext.eq ja jb)
		| _ -> false
	) ~printer: (string_of_response ~printer)

	let ok = let open Server in function
		| OK r -> r
		| r -> assert_failure ("Expected OK, got " ^ (string_of_response r))

	let assert_ok = let open Server in function
		| OK r -> ()
		| r -> assert_failure ("Expected OK, got " ^ (string_of_response r))
end

module Json_response = struct
	let assert_equal = Response.assert_equal ~cmp:J.eq ~printer:J.to_string
end

let test_username = "test"
let user_token = lazy (
	let password = "secret" in
	log#debug "creating test user...";
	let response = post_json
		~data:(`Assoc [("user",`String test_username); ("password", `String password)])
		(Server.path ["auth"; "signup"]) |> Response.ok
	in
	J.mandatory J.get_field "token" response
)

let core_to_db core = Store.build_t core []
let get_server_db () =
	get_json ~token:(Lazy.force user_token)
		(Server.path ["db"])
		|> Response.ok
		|> Store.Format.core_of_json
		|> core_to_db

module Store = struct
	include Store
	let assert_equal = assert_equal ~cmp:Store.eq ~printer:Store.to_json_string
end

(* module-wide setup *)
let (>::) desc test =
	let initial_writer = !Logging.current_writer in
	let setup ctx =
		Logging.current_writer := (fun _dest str -> logf ctx `Info "%s" str);
		log#info "Wiping user db: %s" test_username;
		post_json
		~data:(`Assoc ["user", `String test_username])
		(Server.path ["ctl"; "reset_db"])
		|> Response.assert_ok;
		()
	in
	let teardown () _ = Logging.current_writer := initial_writer in
	desc >:: (fun ctx ->
		bracket setup teardown ctx;
		test ctx
	)

let suite = "sync" >:::
	let db_path = (Server.path ["db"]) in
	let save_db db =
		let response = Sync.sync_db ~token:(Lazy.force user_token) db |> Lwt_main.run in
		response |> Response.assert_ok;
		log#debug "got sync response: %a" J.print (response |> Response.ok);
		get_server_db () |> Store.assert_equal db
	in

	let default_domain = Store.default (Store.empty) in

	let sync_db ~db ~expected_result =
		(* first, we check that the sync_result is the same *)
		Sync.sync_db ~token:(Lazy.force user_token) db
			|> Lwt_main.run
			|> Response.ok
			|> (fun json ->
				let version = json |> J.int_field "version" in
				match version with
				| Some v when v = Store.(db.core.version) ->
					(* version is the same, expected should == db *)
					db |> Store.assert_equal expected_result;
				| _ -> json
					|> Store.Format.core_of_json
					|> core_to_db
					|> Store.assert_equal expected_result
			);

		(* fetch a fresh version from the server and check that, too *)
		get_server_db () |> Store.assert_equal expected_result
	in

	[
	"when not authorized" >::: [
		"getting db" >:: (fun _ ->
			get_json db_path |> Json_response.assert_equal (Server.Unauthorized (Some "Permission denied"))
		);

		"posting db" >:: (fun _ ->
			post_json ~data:(Store.empty |> Store.to_json) db_path |> Json_response.assert_equal (Server.Unauthorized (Some "Permission denied"))
		);
	];
	"initial DB" >:: (fun _ ->
		let db = get_json ~token:(Lazy.force user_token) db_path |> Response.ok in
		Store.parse_json db |> Store.assert_equal Store.empty
	);

	"saving changes stores db and returns new core" >:: (fun _c ->
		let open Store in
		let entry = (Domain {
			domain="example.com";
			note = Some "it's a secret!";
			suffix = Some "1";
			length = 23;
		}) in
		let db = {empty with changes = [Create entry; Default (`Length 12)]} in

		let expected_core = {
			version = 1;
			records = StringMap.empty |> StringMap.add "example.com" entry;
			defaults = { default_length = 12; };
		} in

		sync_db ~db ~expected_result:(core_to_db expected_core)
	);

	"saving non-applicable changes discards them" >:: (fun _c ->
		let open Store in
		let db = empty in
		let db = {db with changes = [
			(Delete "not-present.com");
			(Edit ("also-not-here", (`Domain [(`Length 20)])));
		]} in
		sync_db ~db ~expected_result:{empty with
			core = { empty.core with version = succ empty.core.version } }
	);

	"version mismatch" >:::
		let open Store in
		let old_core =
			apply_changes {empty_core with version = 5}
				[Create (Domain {default_domain "old.example.com" with note = Some "old note"})]
		in
		let old_db = Store.build_t old_core [] in

		let new_core = apply_changes {empty_core with version = 50}
			[Create (Domain {default_domain "new.example.com" with note = Some "new note"})]
		in
		let new_db = Store.(build_t new_core []) in

		[
		"with no changes" >::: [
			"client is behind" >:: (fun _c ->
				save_db new_db;
				sync_db ~db:old_db ~expected_result:new_db
			);

			"client is ahead" >:: (fun _c ->
				save_db old_db;
				sync_db ~db:new_db ~expected_result:new_db
			);
		];

		"with changes" >:::
		let new_record = Store.(Domain (default_domain "newdomain.com")) in
		let clean_change = Store.(Create new_record) in
		let outdated_change = Store.(Delete "old.example.com") in
		[
			"client is behind" >:: (fun _c ->
				let open Store in
				save_db new_db;

				sync_db
					~db:{old_db with changes = [clean_change; outdated_change]}
					~expected_result:{new_db with core = {
						records = new_db.core.records |> StringMap.add "newdomain.com" new_record;
						defaults = new_db.core.defaults;
						version = new_db.core.version + 1;
					}}
			);

			"client is ahead" >:: (fun _c ->
				let open Store in
				save_db old_db;

				sync_db
					~db:{new_db with changes = [clean_change; outdated_change]}
					~expected_result:{new_db with core = {
						records = new_db.core.records |> StringMap.add "newdomain.com" new_record;
						defaults = new_db.core.defaults;
						version = new_db.core.version + 1;
					}}
			);
		];
	];
]
 
