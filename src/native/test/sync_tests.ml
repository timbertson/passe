open Batteries
open Passe
open OUnit2
module J = Json_ext

let post_json ?token ~data url = Server.post_json ?token ~data url |> Lwt_main.run
let get_json ?token url = Server.get_json ?token url |> Lwt_main.run
let log = Logging.get_logger "sync_tests"

let user_token = lazy (
	let username = "test" in
	let password = "secret" in
	log#debug "creating test user...";
	match post_json ~data:(`Assoc [("user",`String username); ("password", `String password)]) (Server.path ["auth"; "signup"]) with
		| Server.OK response ->
				let open Json_ext in
				let token = mandatory get_field "token" response in
				token
		| Server.Failed (reason, _json) -> failwith ("user signup failed: " ^ reason)
		| Server.Unauthorized _ -> failwith ("user signup failed: Unauthorized")
)

let identity = fun x -> x
let str_eq (a:string) (b:string) = a = b

module Response = struct
	let string_of_response ?printer = let open Server in function
		| OK x -> "OK: " ^ (match printer with Some s -> s x | None -> "[no printer]")
		| Unauthorized x -> "Unauthorized: " ^ (Option.to_string identity x)
		| Failed (x, json) -> "Failed: " ^ x ^ (
			json |> Option.map J.to_string |> Option.map (fun j -> " ["^j^"]") |> Option.default ""
		)

	let assert_equal ~cmp ~printer = assert_equal ~cmp:(fun a b ->
		let open Server in match a, b with
		| OK a, OK b -> cmp a b
		| Unauthorized a, Unauthorized b -> Option.eq str_eq a b
		| Failed (sa,ja), Failed (sb,jb) -> str_eq sa sb && (Option.eq Json_ext.eq ja jb)
		| _ -> false
	) ~printer: (string_of_response ~printer)

	let ok = let open Server in function
		| OK r -> r
		| r -> assert_failure ("Expected OK, got " ^ (string_of_response r))
end

module Json_response = struct
	let assert_equal = Response.assert_equal ~cmp:J.eq ~printer:J.to_string
end

module Store = struct
	include Store
	let assert_equal = assert_equal ~cmp:Store.eq ~printer:Store.to_json_string
end

let suite = "sync" >:::
	let db_path = (Server.path ["db"]) in
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

	"save changes stores db and returns new core" >:: (fun _c ->
		let open Store in
		let db = {empty with core = empty_core}  in
		let entry = (Domain {
			domain="example.com";
			hint = Some "it's a secret!";
			suffix = Some "1";
			length = 23;
		}) in
		let db = update ~db ~original:None (Some entry) in

		let expected_core = {
			version = 1;
			records = StringMap.empty |> StringMap.add "example.com" entry;
		} in

		let post_data = db.changes |> Format.json_of_changes in
		logf _c `Info "Data: %a" J.print post_data;
		let new_core = post_json ~token:(Lazy.force user_token) ~data:(post_data) db_path |> Response.ok |> Store.Format.core_of_json in
		let new_db = Store.build_t new_core [] in
		new_db |> Store.assert_equal (Store.build_t expected_core [])
	);
]
 
