open Common
module J = Json_ext

(* TODO: hide these type implementations behind signature *)

module Make (Server:Server.Sig) = struct
	module Log = (val Logging.log_module "client_auth")

	type username = string
	type uid = string
	type credentials = string * J.json
	type implicit_user = username * uid

	type logged_in_user_state = [
		| `Active_user of credentials
		| `Implicit_user of implicit_user
	]

	type authenticated_user_state = [
		| logged_in_user_state
		| `Saved_user of credentials
		| `Saved_implicit_user of implicit_user
	]

	type online_implicit_auth_state = [
		| `Anonymous
		| `Implicit_user of implicit_user
	]

	type implicit_auth_state = [
		| online_implicit_auth_state
		| `Saved_implicit_user of implicit_user
	]

	type explicit_auth_state = [
		| `Logged_out
		| `Failed_login of username
		| `Saved_user of credentials
		| `Active_user of credentials
	]

	type saved_auth_state = [
		| `Saved_user of credentials
		| `Saved_implicit_user of implicit_user
	]

	type offline_auth_state = [
		| saved_auth_state
		| `Anonymous
	]

	type auth_state = [
		| implicit_auth_state
		| explicit_auth_state
	]

	let string_of_auth_state : (auth_state -> string) = function
		| `Logged_out -> "Logged_out"
		| `Anonymous -> "Anonymous"
		| `Failed_login u -> "Failed_login(" ^ u ^ ")"
		| `Saved_user (u, _) -> "Saved_user(" ^ u ^ ", <creds>)"
		| `Active_user (u, _) -> "Active_user(" ^ u ^ ", <creds>)"
		| `Implicit_user (name,uid) -> "Implicit_user(" ^ name ^", " ^ uid ^ ")"
		| `Saved_implicit_user (name,uid) -> "Saved_implicit_user(" ^ name ^", " ^ uid ^ ")"
	
	let is_explicit (auth: auth_state) = match auth with
		| `Implicit_user _ | `Saved_implicit_user _ | `Anonymous -> false
		| `Logged_out | `Failed_login _ | `Saved_user _ | `Active_user _ -> true
	
	let name_of_state (auth: auth_state) = match auth with
		| `Anonymous | `Logged_out -> None

		| `Failed_login name
		| `Saved_user (name, _)
		| `Active_user (name, _)
		| `Implicit_user (name, _)
		| `Saved_implicit_user (name, _) -> Some name

	let name_of_authenticated (auth: authenticated_user_state) = match auth with
		| `Active_user (name, _)
		| `Saved_user (name,_)
		| `Implicit_user (name, _)
		| `Saved_implicit_user (name, _)
			-> name
	
	let name_of_saved (auth: saved_auth_state) = name_of_authenticated (auth:>authenticated_user_state)

	let token_of_offline (auth: offline_auth_state) = match auth with
		| `Saved_user (_, token) -> Some token
		| `Anonymous | `Saved_implicit_user _ -> None

	let explicit_of_authenticated (auth: authenticated_user_state) = match auth with
		| `Implicit_user _ | `Saved_implicit_user _ -> None

		| `Active_user _ as user -> Some user
		| `Saved_user _ as user -> Some user
	
	let failed_login_of_authenticated (auth: authenticated_user_state) = match auth with
		| `Active_user (name, _) | `Saved_user (name, _) -> `Failed_login name
		| `Saved_implicit_user _ | `Implicit_user _ -> `Anonymous

	let saved_user_of_authenticated (auth: authenticated_user_state) = match auth with
		| `Saved_user creds | `Active_user creds -> `Saved_user creds
		| `Saved_implicit_user user | `Implicit_user user -> `Saved_implicit_user user

	let uid_of_state (auth: auth_state) = match auth with
		| `Anonymous | `Logged_out -> None
		
		| `Failed_login id
		| `Saved_user (id, _)
		| `Active_user (id, _)
		| `Implicit_user (_, id)
		| `Saved_implicit_user (_, id)
			-> Some id

	let uid_of_authenticated (auth: authenticated_user_state) = match auth with
		| `Saved_user (id, _)
		| `Active_user (id, _)
		| `Implicit_user (_, id)
		| `Saved_implicit_user (_, id)
			-> id

	let username_of_logged_in (auth: logged_in_user_state) = match auth with
		| `Active_user (name, _) | `Implicit_user (name,_) -> name

	let token_of_authenticated (auth: authenticated_user_state) = match auth with
		| `Active_user (_, token) | `Saved_user (_, token) -> Some token
		| `Implicit_user _ | `Saved_implicit_user _ -> None
	
	let authenticated_of_user_state (auth: auth_state) = match auth with
		| #authenticated_user_state as state -> Some state
		| _ -> None

	let username_key = "user"

	let payload ~user ~password =
		`Assoc [
			("user", `String user);
			("password", `String password);
		]

	(* implicit_auth urls: *)
	let server_state_url = Server.path ["auth"; "state"]

	(* explicit auth urls *)
	let login_url = Server.path ["auth"; "login"]
	let signup_url = Server.path ["auth"; "signup"]
	let logout_url = Server.path ["auth"; "logout"]
	let token_validate_url = Server.path ["auth"; "validate"]
	let change_password_url = Server.path ["auth"; "change-password"]
	let delete_user_url = Server.path ["auth"; "delete"]

	let parse_credentials (token:J.json) : credentials =
		let user = token
			|> J.get_field username_key
			|> Option.bind (J.as_string)
			|> Option.default_fn (fun () ->
					raise (AssertionError ("no username found in auth token")))
		in (user, token)

	let parse_implicit_user (json:J.json) : implicit_user option =
		match (json |> J.string_field "name", json |> J.string_field "id") with
			| (Some name, Some uid) -> Some (name, uid)
			| (None, None) -> None
			| _mixed ->
					Log.warn (fun m->m "Unable to parse auth state JSON: %s" (J.to_string json));
				None

	let get_response_credentials response =
		let creds = J.get_field "token" response in
		let username = creds |> Option.bind (J.string_field username_key) in
		match (username, creds) with
			| Some user, Some creds -> (user, creds)
			| _ -> raise (AssertionError "credentials doesn't contain a user key")

end
