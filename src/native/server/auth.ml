open Passe
open Common
module J = Json_ext
open Lwt
module Base64 = B64

type 'a either = [
	| `Success of 'a
	| `Failed of string
]

let re_newline = Str.regexp "\n"
let lines_stream stream =
	let buf : (string list * string option) ref = ref ([],None) in
	Lwt_stream.from (fun () ->
		match !buf with
			| (line :: lines, partial) ->
				(* pop a complete line *)
				buf := (lines, partial);
				return (Some line)
			| ([], partial) ->
				let rec read_more () =
					lwt chunk = Lwt_stream.get stream in
					match chunk with
						| None ->
								buf := ([], None);
								return partial
						| Some chunk -> begin
							let lines = Str.split_delim re_newline chunk in
							let partial_text = match partial with Some p -> p | None -> "" in
							match lines with
								| [] -> failwith "impossible"
								| tail :: [] ->
										(* accumulate and keep trying *)
										buf := ([], Some (partial_text^tail));
										read_more ()
								| first :: lines ->
										(* TODO: just slice the list, rather than reversing it twice *)
										let lines = List.rev lines in
										let last = List.hd lines in
										let lines = List.rev (List.tl lines) in
										buf := (lines, Some last);
										return (Some (partial_text^first))
						end
				in
				read_more ()
	)


module type Sig = sig
	module Fs : Filesystem.Sig
	module Token : sig
		type sensitive_token
		type stored_token
		val of_json : J.json -> sensitive_token
		val to_json : sensitive_token -> J.json
	end
	module User : sig
		type sandstorm_user
		type db_user
		type uid
		type t = [
			| `Sandstorm_user of sandstorm_user
			| `DB_user of db_user
		]
		val display_name : t -> string
		val uid : t -> uid
		val uid_db : db_user -> uid
		val string_of_uid : uid -> string
		val uid_of_string : string -> uid
		val sandstorm_user : name:string -> id:string -> unit -> sandstorm_user
		val json_of_sandstorm : sandstorm_user -> J.json
	end
	class storage : Fs.t -> string -> object
		method modify :
			(User.db_user Lwt_stream.t -> (User.db_user -> unit Lwt.t) -> Fs.write_commit Lwt.t)
			-> unit Lwt.t
		method read : 'a. (User.db_user Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t
	end
	val signup : storage:storage -> string -> string -> Token.sensitive_token either Lwt.t
	val login : storage:storage -> string -> string -> Token.sensitive_token either Lwt.t

	val validate : storage:storage -> Token.sensitive_token -> User.db_user option Lwt.t
	val logout : storage:storage -> Token.sensitive_token -> unit Lwt.t
	val change_password : storage:storage -> User.db_user -> string -> string -> Token.sensitive_token option Lwt.t
	val delete_user : storage:storage -> User.db_user -> string -> bool Lwt.t
end

module Make (Clock:V1.CLOCK) (Hash_impl:Hash.Sig) (Fs:Filesystem.Sig) = struct
	module Fs = Fs
	module Log = (val Logging.log_module "auth")

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
		val of_cstruct : Cstruct.t -> t
		val to_string : t -> string
		val to_json : t -> J.json
		val lift : (string -> 'a) -> t -> 'a
		val field : string -> J.json -> t option
		val length : t -> int
	end
	module Bytes: BYTES = struct
		type t = Cstruct.t
		let of_cstruct b = b
		let to_string b = Base64.encode (Cstruct.to_string b)
		let of_string s = Cstruct.of_string (Base64.decode s)
		let to_json b = `String (to_string b)
		let lift fn bytes = fn (Cstruct.to_string bytes)
		let field k o : t option = J.string_field k o |> Option.map of_string
		let length b = Cstruct.len b
	end

	module Rng = Nocrypto.Rng

	let random_bytes len: Bytes.t Lwt.t =
		let bytes = Rng.generate len in
		(* TODO: remove lwt.t if this stays sync *)
		return (Bytes.of_cstruct bytes)

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
			let now = Clock.time () in
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
		type uid = string
		type password_info = {
			salt:Bytes.t;
			iterations:int;
			alg:string;
		}
		type stored_password = (string, password_info) stored

		type db_user = {
			name: string;
			password: stored_password;
			active_tokens: Token.stored_token list;
		}

		type sandstorm_user = {
			id: string;
			display_name: string;
		}

		type t = [
			| `Sandstorm_user of sandstorm_user
			| `DB_user of db_user
		]

		let uid_of_string x = x
		let string_of_uid x = x

		let uid_db u = u.name
		let uid = function
			| `DB_user u -> uid_db u
			| `Sandstorm_user u -> u.id

		let json_of_sandstorm u : J.json =
			`Assoc [ "name", `String u.display_name; "id", `String u.id ]

		let display_name = function
			| `DB_user u -> u.name
			| `Sandstorm_user u -> u.display_name

		let sandstorm_user ~name ~id () =
			{ display_name=name; id=id }

		let outdated_password p =
			let meta = p.stored_metadata in
			(
				meta.iterations <> Password.iterations ||
				(Bytes.length meta.salt) <> Password.salt_length ||
				meta.alg <> Hash_impl.alg
			)

		let hash_password ~salt ~iterations password =
			Bytes.lift (fun seed ->
				Hash_impl.hash ~count:iterations ~seed password
			) salt |> Hash_impl.serialize

		let verify stored password =
			match stored with
				| { stored_contents; stored_metadata = { iterations; salt; _; }} ->
					let hashed = hash_password ~salt ~iterations password in
					hashed = stored_contents

		let latest_password_format password : stored_password Lwt.t =
			lwt salt = random_bytes Password.salt_length in
			let iterations = Password.iterations in
			let hashed_password = hash_password ~salt ~iterations password in
			return {
				stored_contents = hashed_password;
				stored_metadata = {
					salt = salt;
					iterations = iterations;
					alg = Hash_impl.alg;
				}
			}

		let gen_token user password : (db_user * Token.sensitive_token) option Lwt.t =
			let token_alg = user.password.stored_metadata.alg in
			(* Important: use the hash algorithm corresponding to the _stored_ password.
			 * If it's an old algo, we'll update it (after validating)
			 *)
			let (module Hash_impl) = Hash.select token_alg in
			if verify user.password password then (
				lwt password = if outdated_password user.password then (
					Log.info (fun m->m "upgrading password for %s" user.name);
					latest_password_format password
				) else return user.password in
				lwt new_token = Token.create ~username:user.name () in
				let tokens = Token.hash new_token :: user.active_tokens in

				(* limit number of historical tokens to 10 per user *)
				let tokens = if List.length tokens > max_tokens
					then BatList.take max_tokens tokens
					else tokens
				in
				let user = { user with active_tokens = tokens; password = password } in
				return (Some (user, new_token))
			) else return_none

		let password_of_json j = {
			stored_metadata = {
				salt = mandatory Bytes.field "salt" j;
				iterations = mandatory J.int_field "iterations" j;
				alg = J.string_field "alg" j |> Option.default "bcrypt";
			};
			stored_contents = mandatory J.string_field "contents" j;
		}

		let json_of_password p =
			let info = p.stored_metadata in
			`Assoc [
				("salt", info.salt |> Bytes.to_json);
				("iterations", `Int info.iterations);
				("contents", `String p.stored_contents);
				("alg", `String info.alg);
			]
		
		
		let of_json j =
			let name =  mandatory J.string_field "name" j in
			{
				name = name;
				password = mandatory J.get_field "password" j |> password_of_json;
				active_tokens = mandatory J.list_field "tokens" j |> List.map (Token.of_stored_json ~username:name);
			}

		let to_json (u:db_user) =
			`Assoc [
				("name", `String u.name);
				("password", json_of_password u.password);
				("tokens", `List (u.active_tokens |> List.map (Token.to_stored_json)));
			]
		
		let create ~username password =
			validate_username username;
			lwt token = Token.create ~username () in
			lwt stored_password = latest_password_format password in
			let stored_token = Token.hash token in
			let user = {
				name = username;
				password = stored_password;
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

		let remove_token user (token:Token.sensitive_token) : db_user option =
			_find_token user token |> Option.map (fun tok ->
				{ user with active_tokens = user.active_tokens |> List.filter (fun t -> t != tok) }
			)

	end

	class storage fs filename =
		let lock = Lwt_mutex.create () in
		let () = Log.info (fun m->m "storage located at %s" filename) in

		object (self)
		method modify (fn:User.db_user Lwt_stream.t -> (User.db_user -> unit Lwt.t) -> Fs.write_commit Lwt.t) =
			Lwt_mutex.with_lock lock (fun () ->
				let now = Clock.time () in
				let num_written = ref 0 in
				let (output_chunks, output) = Lwt_stream.create_bounded 10 in

				let write_user user =
					(* whenever we save a user, trim their tokens to just those
						* which have not yet expired *)
					let user = {
						user with User.active_tokens = user.User.active_tokens |> List.filter
							(fun tok -> tok.stored_metadata.Token.expires > now)
					} in
					let json = User.to_json user in
					let line = J.to_single_line_string json in
					Log.debug (fun m->m "writing output user... %s" (user.User.name));
					num_written := !num_written + 1;
					output#push (`Output (line^"\n"))
				in

				(* stream output while processing input *)
				Lwt.join [
					Fs.write_file_s fs filename output_chunks;
					(
						try_lwt
							lwt commit = self#_read (fun users -> fn users write_user) in
							match commit with
								| `Rollback ->
										Log.debug (fun m->m "discarding changes");
										output#push `Rollback
								| `Commit ->
										Log.debug (fun m->m "committing changes");
										return_unit
						with e ->
							(* write_file_s never seems to terminate if it is cancelled
							 * before seeing any output. The exception should cancel
							 * the writing _anyway_, but ths can't hurt *)
							lwt () = output#push `Rollback in
							raise e
						finally (
							Log.debug (fun m->m "closing output");
							return output#close
						)
					)
				]
			)

		method read : 'a. (User.db_user Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t = fun fn ->
			Lwt_mutex.with_lock lock (fun () -> self#_read fn)

		(* NOTE: must only be called while holding `lock` *)
		method private _read : 'a. (User.db_user Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t = fun fn ->
			let opened = ref false in
			(Fs.read_file_s fs filename) (fun s ->
				let lines = lines_stream s in
				let db_users = lines |> Lwt_stream.filter_map (fun line ->
					Option.non_empty ~zero:"" line |>
						Option.map (fun line -> J.from_string line |> User.of_json)
				) in
				lwt users =
					try_lwt
						(* force evaluation of the first line, so we get ENOENT immediately *)
						lwt (_:string option) = Lwt_stream.peek lines in
						opened := true;
						return db_users
					with Fs.Error (Fs.ENOENT _) when not !opened -> (
						Log.debug (fun m->m "Ignoring missing user DB");
						return (Lwt_stream.of_list [])
					)
				in
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
		lwt () = storage#modify (fun users write_user ->
			try_lwt
				lwt () = users |> Lwt_stream.iter_s (fun user ->
					if user.User.name = username then raise Conflict ;
					write_user user
				) in
				lwt (new_user, token) = User.create ~username password in
				lwt () = write_user new_user in
				result := Some (`Created token);
				return `Commit
			with e -> begin
				result := (match e with
					| Conflict -> Some `Conflict
					| Invalid_username -> Some `Invalid_username
					| e ->
						Log.info (fun m->m "Unexpected error in account creation: %s" (Printexc.to_string e));
						None
				);
				return `Rollback
			end
		) in
		return (match !result with
			| Some (`Created rv) -> `Success rv
			| Some `Invalid_username -> `Failed "Username must start with a letter and contain only letters, numbers dashes and underscores"
			| None | Some `Conflict -> `Failed "Account creation failed"
		)


	let get_user ~(storage:storage) username : User.db_user option Lwt.t =
		storage#read (fun users ->
			users |> Lwt_stream.find (fun user -> user.User.name = username)
		)

	exception Invalid_credentials

	let login ~(storage:storage) username password : Token.sensitive_token either Lwt.t =
		let created = ref None in
		lwt () = storage#modify (fun users write_user ->
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
				(* don't bother writing unless we've added a token *)
				return (match !created with Some _ -> `Commit | None -> `Rollback)
			with Invalid_credentials -> return `Rollback
		) in
		return (match !created with
			| Some rv -> `Success rv
			| None -> `Failed "Authentication failed")

	let validate ~(storage:storage) token : User.db_user option Lwt.t =
		let info = token.sensitive_metadata in
		let expires = info.Token.expires in
		if expires < Clock.time () then
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
		if expires < Clock.time () then
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
					return `Commit
				with Invalid_credentials -> return `Rollback
			)
		)

	let change_password ~(storage:storage) user old new_password : Token.sensitive_token option Lwt.t =
		match_lwt User.gen_token user old with
			| None -> return None
			| Some _ ->
				let ret = ref None in
				lwt () = storage#modify (fun users write_user ->
					lwt () = users |> Lwt_stream.iter_s (fun db_user ->
						lwt db_user = if db_user.User.name = user.User.name then (
							lwt new_user,token = User.create ~username:db_user.User.name new_password in
							ret := Some token;
							let open User in

							(* fields explicitly copied because we need to not overwrite any
							* future properties we add *)
							return {
								name=db_user.name;
								password=new_user.password;
								active_tokens=new_user.active_tokens;
							}
						) else return db_user in
						write_user db_user
					) in
					return (match !ret with Some _ -> `Commit | None -> `Rollback)
				) in
				return !ret

	let delete_user ~(storage:storage) user password : bool Lwt.t =
		match_lwt User.gen_token user password with
			| None -> return false
			| Some _ ->
				let deleted = ref false in
				lwt () = storage#modify (fun users write_user ->
					lwt () = users |> Lwt_stream.iter_s (fun db_user ->
						if db_user.User.name = user.User.name then (
							deleted := true;
							return_unit
						) else
							write_user db_user
					) in
					return (if !deleted then `Commit else `Rollback)
				) in
				return !deleted

end
