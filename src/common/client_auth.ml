open Lwt
open Server
open Sync
open Common
open Either
module J = Json_ext

let payload ~user ~password =
	`Assoc [
		("user", `String user);
		("password", `String password);
	]

let submit url data : (string, credentials) either Lwt.t =
	lwt response = post_json ~data url in
	return begin match response with
	| OK response ->
		let creds = J.get_field "token" response in
		let username = creds |> Option.bind (J.string_field username_key) in
		(match (username, creds) with
			| Some user, Some creds -> Right (user, creds)
			| _ -> raise (AssertionError "credentials doesn't contain a user key")
		)
	| Failed (message, _) -> Left message
	| Unauthorized _ -> assert false
end

let login ~user ~password = submit login_url (payload ~user ~password)
