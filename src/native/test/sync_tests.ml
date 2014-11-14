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

let suite = "sync" >::: [
	"get DB" >:: (fun _ ->
		match get_json ~token:(Lazy.force user_token) (Server.path ["db"]) with
		| Server.OK db -> failwith "TODO: make some assertions"
		| Server.Unauthorized _ -> failwith "Unauthorized"
		| Server.Failed (reason, _json) -> failwith ("Failed: "^reason)
	);
]
 
