open Lwt
open Passe
open Common
module J = Json_ext
module Str = Re_str

let re_newline = Str.regexp "\n"
let lines_stream stream =
	let state : (string list * string option) option ref = ref (Some ([],None)) in
	Lwt_stream.from (fun () ->
		match !state with
			| None -> return_none
			| Some (line :: lines, partial) ->
				(* pop a complete line *)
				state := Some (lines, partial);
				return (Some (Ok line))
			| Some ([], partial) ->
				let rec read_more () =
					let%lwt chunk = Lwt_stream.get stream in
					match chunk with
						| None ->
								state := Some ([], None);
								return (partial |> Option.map R.ok)
						| Some (Error _ as err) ->
								state := None; (* stop after this *)
								return (Some err)
						| Some (Ok chunk) -> begin
							let lines = Str.split_delim re_newline chunk in
							let partial_text = match partial with Some p -> p | None -> "" in
							match lines with
								| [] -> failwith "impossible"
								| tail :: [] ->
										(* accumulate and keep trying *)
										state := Some ([], Some (partial_text^tail));
										read_more ()
								| first :: lines ->
										(* TODO: just slice the list, rather than reversing it twice *)
										let lines = List.rev lines in
										let last = List.hd lines in
										let lines = List.rev (List.tl lines) in
										state := Some (lines, Some last);
										return (Some (Ok (partial_text^first)))
						end
				in
				read_more ()
	)

type cancel = [`Cancel]
let rollback e = `Rollback e

module type Sig = sig
	module Store : Dynamic_store.Sig
	module Clock : Mirage_clock.PCLOCK

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
	class storage : Store.t -> Path.relative -> object
		method modify : 'a.
			(User.db_user list -> (User.db_user list option * 'a) Lwt.t) (* modify *)
			-> ('a, Error.t) result Lwt.t
		method modify_user : 'a.
			string
			-> (User.db_user -> (User.db_user * 'a, Error.t) result Lwt.t)
			-> ('a option, Error.t) result Lwt.t
		method read: (User.db_user list, Error.t) Lwt_r.t
	end

	val signup : storage:storage -> string -> string -> (Token.sensitive_token, Error.t) result Lwt.t
	val login : storage:storage -> string -> string -> (Token.sensitive_token, Error.t) result Lwt.t

	val validate : storage:storage -> Token.sensitive_token -> (User.db_user option, Error.t) result Lwt.t
	val logout : storage:storage -> Token.sensitive_token -> (unit, Error.t) result Lwt.t
	val change_password : storage:storage -> User.db_user -> string -> string
		-> (Token.sensitive_token option, Error.t) result Lwt.t
	val delete_user : storage:storage -> User.db_user -> string -> (bool, Error.t) result Lwt.t
end

module Make (Clock:Mirage_clock.PCLOCK) (Hash_impl:Hash.Sig) (Store:Dynamic_store.Sig) = struct
	module Store = Store
	module Clock = Clock
	module Log = (val Logging.log_module "auth")
	let time () = Clock.now_d_ps () |> Ptime.v

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

	type date = Ptime.t
	let two_weeks = Ptime.Span.v (14, 0L)

	let day_of_date : Ptime.t -> int = fun d ->
		d |> Ptime.to_span |> Ptime.Span.to_d_ps |> Tuple.fst

	let date_of_day : int -> Ptime.t = fun d ->
		Ptime.v (d, 0L)

	module type BYTES = sig
		type t
		val of_cstruct : Cstruct.t -> t
		val to_json : t -> J.json
		val lift : (string -> 'a) -> t -> 'a
		val field : string -> J.json -> t option
		val length : t -> int
	end
	module Bytes: BYTES = struct
		type t = Cstruct.t
		let of_cstruct b = b
		let to_string b = Base64.encode (Cstruct.to_string b) |> Error.raise_result
		let of_string s = Cstruct.of_string (Base64.decode s |> Error.raise_result)
		let to_json b = `String (to_string b)
		let lift fn bytes = fn (Cstruct.to_string bytes)
		let field k o : t option = J.string_field k o |> Option.map of_string
		let length b = Cstruct.length b
	end

	module Rng = Mirage_crypto_rng

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

		let serialize_expires exp = `Int (day_of_date exp)

		let parse_expires obj =
			J.int_field "expires" obj |> Option.default 0 |> date_of_day

		let of_stored_json ~username j = {
			stored_metadata = {
				user = username;
				expires = parse_expires j;
			};
			stored_contents = mandatory J.string_field "contents" j;
		}

		let of_json j : sensitive_token = {
			sensitive_metadata = {
				user = mandatory J.string_field "user" j;
				expires = parse_expires j;
			};
			sensitive_contents = mandatory Bytes.field "contents" j;
		}

		let hash (t:sensitive_token) : stored_token =
			let sha256 : string -> string = fun str ->
				str
					|> Cstruct.of_string
					|> Mirage_crypto.Hash.SHA256.digest
					|> Hex.of_cstruct
					|> Hash.string_of_hex
			in
			let hashed_contents = Bytes.lift sha256 t.sensitive_contents in
			{
				stored_contents = hashed_contents;
				stored_metadata = t.sensitive_metadata;
			}

		let to_stored_json (t:stored_token) =
			let info = t.stored_metadata in
			`Assoc [
				("expires", serialize_expires info.expires);
				("contents", `String t.stored_contents);
			]

		let to_json (t:sensitive_token) =
			let info = t.sensitive_metadata in
			`Assoc [
				("user", `String info.user);
				("expires", serialize_expires info.expires);
				("contents", Bytes.to_json t.sensitive_contents);
			]
		
		let create ~username () : sensitive_token Lwt.t =
			let now = Clock.now_d_ps () |> Ptime.v in
			let expires : Ptime.t = Ptime.add_span now two_weeks |> Option.force in
			let%lwt contents = random_bytes 20 in
			return {
				sensitive_contents = contents;
				sensitive_metadata = {
					expires = expires;
					user = username;
				}
			}

	end


	let valid_username_pattern = Str.regexp "^[a-zA-Z][-_a-zA-Z0-9]+$"
	let validate_username name =
		if (Str.string_match valid_username_pattern name 0)
			then Ok name
			else Error (`Invalid "username")

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
			let%lwt salt = random_bytes Password.salt_length in
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
			if verify user.password password then (
				let token_alg = user.password.stored_metadata.alg in
				(* Important: use the hash algorithm corresponding to the _stored_ password.
				 * If it's an old algo, we'll update it (after validating)
				 *)
				let (module Hash_impl) = Hash.select token_alg in
				let%lwt password = if outdated_password user.password then (
					Log.info (fun m->m "upgrading password for %s" user.name);
					latest_password_format password
				) else return user.password in
				let%lwt new_token = Token.create ~username:user.name () in
				let tokens = Token.hash new_token :: user.active_tokens in

				(* limit number of historical tokens to 10 per user *)
				let tokens = if List.length tokens > max_tokens
					then List.take max_tokens tokens
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
			let name = mandatory J.string_field "name" j in
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
			return (validate_username username) |> Lwt_r.bind (fun username ->
				Token.create ~username () |> Lwt.bindr (fun token ->
					latest_password_format password |> Lwt.bindr (fun stored_password ->
						let stored_token = Token.hash token in
						let user = {
							name = username;
							password = stored_password;
							active_tokens = [stored_token];
						}
						in
						return (Ok (user, token))
					)
				)
			)

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

		let matches_name: string -> db_user -> bool =
			fun name candidate -> candidate.name = name
	end

	class storage fs path =
		let () = Log.info (fun m->m "storage located at %a" Path.pp path) in

		let read_with_write_permission: 'a.
			(Lock.proof -> User.db_user list -> ('a, Error.t) result Lwt.t)
			-> ('a, Error.t) result Lwt.t
		= fun fn ->
			Store.read_for_writing fs path (fun proof contents ->
				match contents with
					| Ok None ->
						Log.debug (fun m->m "Ignoring missing user DB");
						fn proof []
					| Ok (Some contents) ->
						let lines = String.split_on_char '\n' contents |> List.filter ((!=) "") in
						let db_users : User.db_user list = lines
							|> List.map (fun line -> J.from_string line |> User.of_json)
						in
						fn proof db_users
					| Error _ as e -> Lwt.return e
			)
		in

		object (self)
		method modify: 'a.
			(User.db_user list -> (User.db_user list option * 'a) Lwt.t) (* modify *)
			-> ('a, Error.t) Lwt_r.t
		= fun modify_fn -> (
			let now = time () in
			let double_expiry = Ptime.Span.add two_weeks two_weeks in
			let max_expiry = Ptime.add_span now double_expiry |> Option.force in

			let serialize_user user =
				(* whenever we save a user, trim their tokens to just those
					* which have not yet expired (with a sanity check that discards
					* tokens whose expiry is too far in the future, too) *)
				let active_tokens = user.User.active_tokens |> List.filter (fun tok ->
					let expires = tok.stored_metadata.Token.expires in
					Ptime.is_later expires ~than:now && Ptime.is_earlier expires ~than:max_expiry
				) in
				let user = { user with User.active_tokens = active_tokens } in
				let json = User.to_json user in
				let line = J.to_single_line_string json in
				Log.debug (fun m->m "writing output user `%s` with %d tokens"
					(user.User.name)
					(List.length active_tokens)
				);
				line
			in

			read_with_write_permission (fun proof users ->
				let%lwt (modified, result) = modify_fn users in
				let written: (unit, Error.t) Lwt_r.t = (match modified with
					| Some modified -> (
						let output = modified |> List.map serialize_user |> String.concat "\n" in
						Store.write fs path ~proof output
					)
					| None -> Lwt_r.return ()
				) in
				written |> Lwt_r.map (fun () -> result)
			)
		)

		method modify_user : 'a 'err.
			string
			-> (User.db_user -> (User.db_user * 'a, Error.t) result Lwt.t)
			-> ('a option, Error.t) result Lwt.t
		= fun username modify_user_fn -> self#modify (fun all_users ->
			let rec loop = fun ~processed_rev -> function
				| candidate :: unprocessed_users -> (
					if User.matches_name username candidate then (
						modify_user_fn candidate |> Lwt.map (function
							| Ok (updated, ret) ->
								let full_updated_users = (List.rev processed_rev @ [updated] @ unprocessed_users) in
								(Some full_updated_users, Ok (Some ret))
							| Error e -> (None, Error e)
						)
					)
					else loop ~processed_rev:(candidate :: processed_rev) unprocessed_users
				)
				| [] ->
					(* not found *)
					Lwt.return (None, Ok None)
			in
			
			loop ~processed_rev:[] all_users
		) |> Lwt.map (R.flatten)

		method read: (User.db_user list, Error.t) Lwt_r.t =
			read_with_write_permission (fun _proof users -> Lwt_r.return users)
	end

	let signup ~(storage:storage) username password : (Token.sensitive_token, Error.t) result Lwt.t = (
		let modification: ((Token.sensitive_token, Error.t) result, Error.t) result Lwt.t = storage#modify (fun all_users ->
			match List.find_opt (User.matches_name username) all_users with
				(* Don't modify users and return a failure *)
				| Some existing -> Lwt.return (None, Error (`Failed "Account creation failed"))
				| None -> (
					User.create ~username password |> Lwt.map (function
						| Ok (new_user, token) ->
							(* insert new user and return its token *)
							(Some (new_user :: all_users), Ok token)
						| Error e -> (None, Error e) (* don't write an updated user list and return an error *)
					)
				)
		) in

		modification
			|> Lwt.map (R.flatten) (* flatten Ok(Error ...) *)
			|> Lwt.map (R.on_error (fun e ->
				Log.warn (fun m->m "Unexpected error in account creation: %a" Error.pp e)
			))
	)

	let get_user ~(storage:storage) username : (User.db_user option, Error.t) result Lwt.t =
		storage#read |> Lwt_r.map (fun users ->
			users |> List.find_opt (function user -> user.User.name = username)
		)

	let login ~(storage:storage) username password : (Token.sensitive_token, Error.t) result Lwt.t =
		let login_failed () = Error (`Failed "Authentication failed") in
		storage#modify_user username (fun user ->
			User.gen_token user password |> Lwt.map (function
				| Some (user, token) -> Ok (user, token)
				| None -> login_failed ()
			)
		) |> Lwt_r.bindR (function
			| None -> login_failed ()
			| Some token -> Ok token
		) |> Lwt.map (R.on_error (fun e ->
				Log.warn (fun m->m "Unexpected error in login: %a" Error.pp e);
		))

	let token_has_expired info =
		let expires = info.Token.expires in
		Ptime.is_earlier expires ~than:(time ())

	let validate ~(storage:storage) token : (User.db_user option, Error.t) result Lwt.t =
		let info = token.sensitive_metadata in
		if token_has_expired info then
			return (Ok None)
		else (
			get_user ~storage (info.Token.user) |> Lwt_r.map (fun user ->
				user |> Option.bind (fun user ->
					if User.validate user token
						then Some user
						else None
				)
			)
		)

	let logout ~(storage:storage) token : (unit, Error.t) result Lwt.t =
		let info = token.sensitive_metadata in
		if token_has_expired info then
			return (Ok ())
		else (
			let username = info.Token.user in
			storage#modify_user username (fun user ->
				Lwt.return (
					match User.remove_token user token with
						| Some user -> Ok (user, ())
						| None -> Error (`Failed "Invalid credentials")
				)
			) |> Lwt_r.bindR (function
				| Some () -> Ok ()
				| None -> Ok () (* No such user, but that's OK :shrug: *)
			)
		)

	let change_password ~(storage:storage) user old new_password
		: (Token.sensitive_token option, Error.t) result Lwt.t = (
		if User.verify (user.User.password) old then (
			storage#modify_user user.User.name (fun db_user ->
				User.create ~username:db_user.User.name new_password
					|> Lwt_r.map (fun (new_user, token) ->
						let open User in
						(* fields explicitly copied because we need to not overwrite any
						* future properties we add *)
						let db_user = {
							name=db_user.name;
							password=new_user.password;
							active_tokens=new_user.active_tokens;
						} in
						(db_user, token)
					)
			)
		) else (
			return (Ok None)
		)
	)

	let delete_user ~(storage:storage) user password : (bool, Error.t) result Lwt.t =
		if User.verify (user.User.password) password then (
			let is_user = User.matches_name user.User.name in
			storage#modify (fun all_users ->
				let filtered = all_users |> List.filter_map (fun candidate ->
					if is_user candidate then None else Some candidate
				) in
				Lwt.return (
					if filtered = all_users then
						(None, false)
					else
						(Some filtered, true)
				)
			)
		) else (return (Ok false))

end
