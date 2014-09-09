open Lwt
open Common
open Either
module J = Json_ext

type username = string
type credentials = string * J.json
type auth_state =
	| Anonymous
	| Failed_login of username
	| Saved_user of credentials
	| Active_user of credentials

let string_of_auth_state = function
	| Anonymous -> "Anonymous"
	| Failed_login u -> "Failed_login(" ^ u ^ ")"
	| Saved_user (u, _) -> "Saved_user(" ^ u ^ ", <creds>)"
	| Active_user (u, _) -> "Active_user(" ^ u ^ ", <creds>)"

let username_key = "user"

let payload ~user ~password =
	`Assoc [
		("user", `String user);
		("password", `String password);
	]

let login_url = Server.path ["auth"; "login"]
let signup_url = Server.path ["auth"; "signup"]
let logout_url = Server.path ["auth"; "logout"]
let token_validate_url = Server.path ["auth"; "validate"]

let parse_credentials (token:J.json) : credentials =
	let user = token
		|> J.get_field username_key
		|> Option.bind (J.as_string)
		|> Option.default_fn (fun () ->
				raise (AssertionError ("no username found in auth token")))
	in (user, token)


let get_response_credentials response =
	let creds = J.get_field "token" response in
	let username = creds |> Option.bind (J.string_field username_key) in
	match (username, creds) with
		| Some user, Some creds -> (user, creds)
		| _ -> raise (AssertionError "credentials doesn't contain a user key")


