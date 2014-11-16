open Passe
open Common
module J = Json_ext
open Lwt
module Base64 = Batteries.Base64

let log = Logging.get_logger "auth"

let mandatory = J.mandatory

type ('c, 't) stored = {
	stored_contents: 'c;
	stored_metadata: 't;
}

type ('c, 't) sensitive = {
	(* for in-memory use, not to be stored *)
	sensitive_contents: 'c;
	sensitive_metadata: 't;
}

let stored_eq eq a b =
	a.stored_contents = b.stored_contents && eq a.stored_metadata b.stored_metadata

type date = float
let one_day = 1.0 *. (60.0 *. 60.0 *. 24.0)

module type BYTES = sig
	type t
	val raw : string -> t
	val to_string : t -> string
	val to_json : t -> J.json
	val lift : (string -> 'a) -> t -> 'a
	val field : string -> J.json -> t option
end
module Bytes: BYTES = struct
	type t = string
	let raw b = b
	let to_string b = Base64.str_encode b
	let of_string s = Base64.str_decode s
	let to_json b = `String (to_string b)
	let lift fn bytes = fn bytes
	let field k o : t option = J.string_field k o |> Option.map of_string
end


let random_bytes len: Bytes.t Lwt.t =
	Lwt_io.with_file ~mode:Lwt_io.input "/dev/urandom" (fun f ->
		let buf = String.create len in
		lwt () = Lwt_io.read_into_exactly f buf 0 len in
		return (Bytes.raw buf)
	)

module Password = struct
	let iterations = 6
	let salt_length = 16
end

module Token = struct
	type info = {
		user: string;
		expires: date;
	}
	type stored_token = (string, info) stored
	type sensitive_token = (Bytes.t, info) sensitive

	let of_stored_json ~username j = {
		stored_metadata = {
			user = username;
			expires = mandatory J.float_field "expires" j;
		};
		stored_contents = mandatory J.string_field "contents" j;
	}

	let of_json j : sensitive_token = {
		sensitive_metadata = {
			user = mandatory J.string_field "user" j;
			expires = mandatory J.float_field "expires" j;
		};
		sensitive_contents = mandatory Bytes.field "contents" j;
	}

	let hash (t:sensitive_token) : stored_token =
		let sha256 : string -> string = (Sha256.to_hex % Sha256.string) in
		let hashed_contents = Bytes.lift sha256 t.sensitive_contents in
		{
			stored_contents = hashed_contents;
			stored_metadata = t.sensitive_metadata;
		}

	let to_stored_json (t:stored_token) =
		let info = t.stored_metadata in
		`Assoc [
			("expires", `Float info.expires);
			("contents", `String t.stored_contents);
		]

	let to_json (t:sensitive_token) =
		let info = t.sensitive_metadata in
		`Assoc [
			("user", `String info.user);
			("expires", `Float info.expires);
			("contents", Bytes.to_json t.sensitive_contents);
		]
	
	let create ~username () : sensitive_token Lwt.t =
		let now = Unix.time () in
		let expires = now +. (14.0 *. one_day) in
		lwt contents = random_bytes 20 in
		return {
			sensitive_contents = contents;
			sensitive_metadata = {
				expires = expires;
				user = username;
			}
		}

end


exception Conflict
exception Invalid_username

let valid_username_pattern = Str.regexp "^[a-zA-Z][-_a-zA-Z0-9]+$"
let validate_username name =
	if not (Str.string_match valid_username_pattern name 0)
	then raise Invalid_username

module User = struct
	let max_tokens = 10
	type password_info = {
		salt:Bytes.t;
		iterations:int;
	}
	type stored_password = (string, password_info) stored

	type t = {
		name: string;
		password: stored_password;
		active_tokens: Token.stored_token list;
	}

	let gen_token user password : (t * Token.sensitive_token) option Lwt.t =
		let stored_hash = user.password.stored_contents |> Bcrypt.hash_of_string in
		if Bcrypt.verify password stored_hash then (
			lwt new_token = Token.create ~username:user.name () in
			let tokens = Token.hash new_token :: user.active_tokens in

			(* limit number of historical tokens to 10 per user *)
			let tokens = if List.length tokens > max_tokens
				then Batteries.List.take max_tokens tokens
				else tokens
			in
			let user = { user with active_tokens = tokens } in
			return (Some (user, new_token))
		) else return_none
	
	let password_of_json j = {
		stored_metadata = {
			salt = mandatory Bytes.field "salt" j;
			iterations = mandatory J.int_field "iterations" j;
		};
		stored_contents = mandatory J.string_field "contents" j;
	}

	let json_of_password p =
		let info = p.stored_metadata in
		`Assoc [
			("salt", info.salt |> Bytes.to_json);
			("iterations", `Int info.iterations);
			("contents", `String p.stored_contents);
		]
	
	
	let of_json j =
		let name =  mandatory J.string_field "name" j in
		{
			name = name;
			password = mandatory J.get_field "password" j |> password_of_json;
			active_tokens = mandatory J.list_field "tokens" j |> List.map (Token.of_stored_json ~username:name);
		}

	let to_json (u:t) =
		`Assoc [
			("name", `String u.name);
			("password", json_of_password u.password);
			("tokens", `List (u.active_tokens |> List.map (Token.to_stored_json)));
		]
	
	let create ~username password =
		validate_username username;
		lwt salt = random_bytes Password.salt_length in
		let iterations = Password.iterations in
		let crypt = fun seed -> Bcrypt.hash ~count:iterations ~seed password
			|> Bcrypt.string_of_hash in
		let hashed_password = Bytes.lift crypt salt in
		let password : stored_password = {
			stored_contents = hashed_password;
			stored_metadata = {
				salt = salt;
				iterations = iterations;
			}
		} in

		lwt token = Token.create ~username () in
		let stored_token = Token.hash token in
		let user = {
			name = username;
			password = password;
			active_tokens = [stored_token];
		}
		in
		return (user, token)


	let _find_token user (token:Token.sensitive_token) : Token.stored_token option =
		if (token.sensitive_metadata.Token.user = user.name) then begin
			let hashed = Token.hash token in
			try
				Some (user.active_tokens |> List.find (fun tok -> tok = hashed))
			with Not_found -> None
		end else None

	let validate user (token:Token.sensitive_token) : bool =
		match _find_token user token with
			| None -> false
			| Some _ -> true

	let remove_token user (token:Token.sensitive_token) : t option =
		_find_token user token |> Option.map (fun tok ->
			{ user with active_tokens = user.active_tokens |> List.filter (fun t -> t != tok) }
		)

end

type 'a either = [
	| `Success of 'a
	| `Failed of string
]

class storage filename =
	let lock = Lwt_mutex.create () in
	let () = log#info "storage located at %s" filename in
	let tmp_name = filename ^ ".tmp" in

	object (self)
	method modify (fn:User.t Lwt_stream.t -> (User.t -> unit Lwt.t) -> bool Lwt.t) =
		Lwt_mutex.with_lock lock (fun () ->
			let modified = ref false in
			lwt () = Lwt_io.with_file ~mode:Lwt_io.output tmp_name (fun output_file ->
				let now = Unix.time () in
				let write_user user =
					(* whenever we save a user, trim their tokens to just those
					 * which have not yet expired *)
					let user = {
						user with User.active_tokens = user.User.active_tokens |> List.filter
							(fun tok -> tok.stored_metadata.Token.expires > now)
					} in
					let json = User.to_json user in
					let line = J.to_single_line_string json in
					Lwt_io.write_line output_file line
				in
				lwt _mod =
					try_lwt
						self#_read (fun users -> fn users write_user)
					with Unix.Unix_error (Unix.ENOENT, "open", filename) ->
						fn (Lwt_stream.of_list []) write_user
				in
				modified := _mod;
				return_unit
			) in
			if !modified
				then Lwt_unix.rename tmp_name filename
				else Lwt.return_unit
		)

	method read : 'a. (User.t Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t = fun fn ->
		Lwt_mutex.with_lock lock (fun () -> self#_read fn)

	(* NOTE: must only be called while holding `lock` *)
	method private _read : 'a. (User.t Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t = fun fn ->
		Lwt_io.with_file ~mode:Lwt_io.input filename (fun f ->
			let lines = Lwt_io.read_lines f in
			let users = Lwt_stream.map (fun line -> J.from_string line |> User.of_json) lines in
			fn users
		)
end

type signup_result = [
	| `Conflict
	| `Invalid_username
	| `Created of J.json
	]

let signup ~(storage:storage) username password : Token.sensitive_token either Lwt.t =
	let result = ref None in
	lwt created = storage#modify (fun users write_user ->
		try_lwt
			lwt () = users |> Lwt_stream.iter_s (fun user ->
				if user.User.name = username then
					raise Conflict
				;
				write_user user
			) in
			lwt (new_user, token) = User.create ~username password in
			lwt () = write_user new_user in
			result := Some (`Created token);
			return_true
		with e -> begin
			result := (match e with
				| Conflict -> Some `Conflict
				| Invalid_username -> Some `Invalid_username
				| _ -> None
			);
			return_false
		end
	) in
	return (match !result with
		| Some (`Created rv) -> `Success rv
		| Some `Invalid_username -> `Failed "Username must start with a letter and contain only letters, numbers dashes and underscores"
		| None | Some `Conflict -> `Failed "Account creation failed"
	)


let get_user ~(storage:storage) username : User.t option Lwt.t =
	storage#read (fun users ->
		users |> Lwt_stream.find (fun user -> user.User.name = username)
	)

exception Invalid_credentials

let login ~(storage:storage) username password : Token.sensitive_token either Lwt.t =
	let created = ref None in
	lwt success = storage#modify (fun users write_user ->
		try_lwt
			lwt () = users |> Lwt_stream.iter_s (fun user ->
				if user.User.name = username then
					match_lwt User.gen_token user password with
						| Some (user, token) ->
							created := Some token;
							write_user user
						| None -> raise Invalid_credentials
				else
					write_user user
				;
			) in
			return_true
		with Invalid_credentials -> return_false
	) in
	return (match !created with
		| Some rv -> `Success rv
		| None -> `Failed "Authentication failed")

let validate ~(storage:storage) token : User.t option Lwt.t =
	let info = token.sensitive_metadata in
	let expires = info.Token.expires in
	if expires < Unix.time () then
		return_none
	else (
		lwt user = get_user ~storage (info.Token.user) in
		return (user |> Option.bind (fun user ->
				if User.validate user token
					then Some user
					else None
		))
	)

let logout ~(storage:storage) token : unit Lwt.t =
	let info = token.sensitive_metadata in
	let expires = info.Token.expires in
	if expires < Unix.time () then
		return_unit
	else (
		let username = info.Token.user in
		storage#modify (fun users write_user ->
			try_lwt
				lwt () = users |> Lwt_stream.iter_s (fun user ->
					if user.User.name = username then (
						match User.remove_token user token with
							| Some user -> write_user user
							| None -> ((raise Invalid_credentials):>unit Lwt.t)
					) else
						write_user user
					;
				) in
				return_true
			with Invalid_credentials -> return_false
		)
	)

