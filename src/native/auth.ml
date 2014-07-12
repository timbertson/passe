module J = Json_ext
open Lwt

let force = Option.force

type 't stored = {
	stored_contents: string;
	stored_metadata: 't;
}

type 't sensitive = {
	(* for in-memory use, not to be stored *)
	sensitive_contents: string;
	sensitive_metadata: 't;
}

let stored_eq eq a b =
	a.stored_contents = b.stored_contents && eq a.stored_metadata b.stored_metadata

type date = float

module Token = struct
	type info = {
		username: string;
		expires: date;
	}
	type stored_token = info stored
	type sensitive_token = info sensitive

	let of_stored_json username j = {
		stored_metadata = {
			username = username;
			expires = J.float_field "expires" j |> force;
		};
		stored_contents = J.string_field "contents" j |> force;
	}

	let to_stored_json (t:stored_token) =
		let info = t.stored_metadata in
		`Assoc [
			("username", `String info.username);
			("contents", `Float info.expires);
			("expires", `String t.stored_contents);
		]

	let to_json (t:sensitive_token) =
		let info = t.sensitive_metadata in
		`Assoc [
			("username", `String info.username);
			("contents", `Float info.expires);
			("expires", `String t.sensitive_contents);
		]
end

module User = struct
	type password_info = {
		salt:string;
		iterations:int;
	}
	type stored_password = password_info stored

	type t = {
		username: string;
		password: stored_password;
		active_tokens: Token.stored_token list;
	}

	let authenticate username password : Token.sensitive_token = {
		sensitive_metadata = { Token.username = username; expires = 0.0 };
		sensitive_contents = "sekret_token"
	}
	
	let password_of_json j = {
		stored_metadata = {
			salt = J.string_field "salt" j |> force;
			iterations = J.int_field "iterations" j |> force;
		};
		stored_contents = J.string_field "contents" j |> force;
	}

	let json_of_password p =
		let info = p.stored_metadata in
		`Assoc [
			("salt", `String info.salt);
			("iterations", `Int info.iterations);
			("contents", `String p.stored_contents);
		]
	
	
	let of_json j =
		let username =  J.string_field "username" j |> force in
		{
			username = username;
			password = J.get_field "password" j |> force |> password_of_json;
			active_tokens = J.list_field "tokens" j |> force |> List.map (Token.of_stored_json username);
		}

	let to_json (u:t) =
		`Assoc [
			("username", `String u.username);
			("password", json_of_password u.password);
			("tokens", `List (u.active_tokens |> List.map (Token.to_stored_json)));
		]
	
	let create username password =
		let password = {
			stored_contents = "TODO";
			stored_metadata = {
				salt = "TODO";
				iterations = 10000;
			}
		} in

		let token = {
			sensitive_contents = "TODO";
			sensitive_metadata = {
				Token.username = username;
				expires = 0.0;
			}
		} in

		let stored_token = {
			stored_contents = "TODO";
			stored_metadata = {
				Token.username = username;
				expires = 0.0;
			}
		} in

		let user = {
			username = username;
			password = password;
			active_tokens = [stored_token];
		}
		in
		(user, token)


end

type signup_response = [
	| `Success of Token.sensitive_token
	| `Failed of string
]

class storage filename =
	let lock = Lwt_mutex.create () in
	let tmp_name = filename ^ ".tmp" in
	object (self)
	method modify (fn:User.t Lwt_stream.t -> (User.t -> unit Lwt.t) -> bool Lwt.t) =
		Lwt_mutex.with_lock lock (fun () ->
			let modified = ref false in
			lwt () = Lwt_io.with_file ~mode:Lwt_io.output tmp_name (fun output_file ->
				let write_user user =
					let json = User.to_json user in
					let line = J.to_single_line_string ~std:true json in
					Lwt_io.write_line output_file line
				in
				lwt _mod = self#_read (fun users -> fn users write_user) in
				modified := _mod;
				return_unit
			) in
			if !modified
				then Lwt_unix.rename tmp_name filename
				else Lwt.return_unit
		)

	method read fn =
		Lwt_mutex.with_lock lock (fun () ->
			self#_read fn
		)

	(* NOTE: must only be called while holding `lock` *)
	method private _read (fn:User.t Lwt_stream.t -> 'a Lwt.t) =
		Lwt_io.with_file ~mode:Lwt_io.input filename (fun f ->
			let lines = Lwt_io.read_lines f in
			let users = Lwt_stream.map (fun line -> J.from_string line |> User.of_json) lines in
			fn users
		)
end

exception Conflict

let signup ~(storage:storage) username password : signup_response Lwt.t =
	let created = ref None in
	lwt success = storage#modify (fun users write_user ->
		try_lwt
			lwt () = users |> Lwt_stream.iter_s (fun user ->
				if user.User.username = username then
					raise Conflict
				;
				write_user user
			) in
			let (new_user, token) = User.create username password in
			lwt () = write_user new_user in
			created := Some token;
			return_true
		with Conflict -> return_false
	) in
	return (match !created with
		| Some rv -> `Success rv
		| None -> `Failed "Account creation failed")

