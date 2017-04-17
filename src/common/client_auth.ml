module J = Json_ext
open Common

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

	let confirmed_uid_of_state (auth: auth_state) = match auth with
		| `Anonymous | `Logged_out | `Failed_login _ -> None

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
			| Some user, Some creds -> Ok (user, creds)
			| _ -> Error "credentials doesn't contain a user key"

	let get_empty_response = function
		| `Assoc [] -> Ok ()
		| other -> Error ("Expected empty response; got " ^ (J.to_string other))

	let get_validity_response response =
		Ok (response |> J.(mandatory bool_field "valid"))

	let parse_implicit_user : implicit_user option Server.response_handler = fun json ->
		match (json |> J.string_field "name", json |> J.string_field "id") with
			| (Some name, Some uid) -> Ok (Some (name, uid))
			| (None, None) -> Ok (None)
			| _mixed ->
				Error ("Unable to parse auth state JSON: %s" ^ (J.to_string json))

	let server_state_api = Server.api ["auth"; "state"] parse_implicit_user

	(* explicit auth urls *)
	let login_api = Server.api ["auth"; "login"] get_response_credentials
	let signup_api = Server.api ["auth"; "signup"] get_response_credentials
	let logout_api = Server.api ["auth"; "logout"] get_empty_response
	let token_validate_api = Server.api ["auth"; "validate"] get_validity_response
	let change_password_api = Server.api ["auth"; "change-password"] get_response_credentials
	let delete_user_api = Server.api ["auth"; "delete"] get_empty_response
end
