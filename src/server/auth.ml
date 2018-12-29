open Lwt
open Passe
open Common
module J = Json_ext
module Str = Re_str
module Base64 = B64

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

let lwt_join2 a b =
	let ra = ref (Obj.magic ()) in
	let rb = ref (Obj.magic ()) in
	Lwt.join [
		(a |> Lwt.map ((:=) ra));
		(b |> Lwt.map ((:=) rb));
	] >>= fun () -> return (!ra, !rb)


type cancel = [`Cancel]
let rollback e = `Rollback e

module type Sig = sig
	module Kv : Kv_store.Sig
	module Clock : Mirage_types.PCLOCK

	type 'err cancel_write = [
		| `Cancel
		| `Rollback of 'err
		| `Write_error of Kv.error
	]

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
	class storage : Clock.t -> Kv.t -> Kv.Path.t -> object
		method clock : Clock.t
		method modify : 'a 'err.
			((User.db_user -> unit Lwt.t) (* write_user *)
				-> (User.db_user, Kv.error) result Lwt_stream.t (* users *)
				-> ('a, 'err cancel_write) result Lwt.t
			)
			-> ('a, 'err cancel_write) result Lwt.t
		method modify_user : 'a 'err.
			string
			-> ?commit:('a option -> ('a, cancel) result)
			-> ((User.db_user -> unit Lwt.t) -> User.db_user -> ('a, 'err cancel_write) result Lwt.t)
			-> ('a, 'err cancel_write) result Lwt.t
		method read: 'a.
			((User.db_user, Kv.error) result Lwt_stream.t -> ('a, Kv.error) result Lwt.t)
			-> ('a, Kv.error) result Lwt.t
	end

	val signup : storage:storage -> string -> string -> (Token.sensitive_token, string) result Lwt.t
	val login : storage:storage -> string -> string -> (Token.sensitive_token, string) result Lwt.t

	val validate : storage:storage -> Token.sensitive_token -> (User.db_user option, Kv.error) result Lwt.t
	val logout : storage:storage -> Token.sensitive_token -> (unit, Kv.error) result Lwt.t
	val change_password : storage:storage -> User.db_user -> string -> string
		-> (Token.sensitive_token option, Kv.error) result Lwt.t
	val delete_user : storage:storage -> User.db_user -> string -> (bool, Kv.error) result Lwt.t
end

module Make (Clock:Mirage_types.PCLOCK) (Hash_impl:Hash.Sig) (Kv:Kv_store.Sig) = struct
	module Kv = Kv
	module Path = Kv.Path
	module Clock = Clock
	module Log = (val Logging.log_module "auth")
	let time clock = Clock.now_d_ps clock |> Ptime.v

	type 'err cancel_write = [
		| `Cancel
		| `Rollback of 'err
		| `Write_error of Kv.error
	]

	let write_error (e:Kv.error) = `Write_error e

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
					|> Nocrypto.Hash.SHA256.digest
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
		
		let create ~clock ~username () : sensitive_token Lwt.t =
			let now = Clock.now_d_ps clock |> Ptime.v in
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
			else Error `Invalid_username

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

		let gen_token ~clock user password : (db_user * Token.sensitive_token) option Lwt.t =
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
				let%lwt new_token = Token.create ~clock ~username:user.name () in
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
		
		let create ~clock ~username password =
			return (validate_username username) |> Lwt_r.bind (fun username ->
				Token.create ~clock ~username () |> Lwt.bindr (fun token ->
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

	end

	class storage clock fs path =
		let () = Log.info (fun m->m "storage located at %a" Path.pp path) in

		let read_with_write_permission (type a)(type err):
			(* cast_read_err is required due to https://caml.inria.fr/mantis/view.php?id=6137 *)
			cast_read_err:(Kv.error -> err)
			-> (Lock.proof -> (User.db_user, Kv.error) result Lwt_stream.t -> (a, err) result Lwt.t)
			-> (a, err) result Lwt.t
		= fun ~cast_read_err fn ->
			Kv.read_for_writing fs path (fun proof response ->
				response |> R.fold (function
					| None ->
						Log.debug (fun m->m "Ignoring missing user DB");
						fn proof (Lwt_stream.of_list [])
					| Some stream ->
						let lines = lines_stream stream in
						let db_users: (User.db_user, Kv.error) result Lwt_stream.t = lines |> Lwt_stream.filter_map (fun line ->
							line |> R.map (fun line ->
								Option.non_empty ~zero:"" line |>
									Option.map (fun line -> J.from_string line |> User.of_json)
							) |> Option_r.unwrap
						) in
						fn proof db_users
				) (fun err -> Lwt.return (Error (cast_read_err err)))
			)
		in
		let default_commit_policy = function
			| Some x -> Ok x
			| None -> Error `Cancel
		in


		object (self)
		method clock = clock
		method modify : 'a 'err.
			((User.db_user -> unit Lwt.t) (* write_user *)
				-> (User.db_user, Kv.error) result Lwt_stream.t (* users *)
				-> ('a, 'err cancel_write) result Lwt.t
			)
			-> ('a, 'err cancel_write) result Lwt.t
		= fun fn -> (
			let now = time clock in
			let double_expiry = Ptime.Span.add two_weeks two_weeks in
			let max_expiry = Ptime.add_span now double_expiry |> Option.force in

			let num_written = ref 0 in
			let (output_chunks, output) = Lwt_stream.create_bounded 10 in

			let write_user user =
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
				Log.debug (fun m->m "writing output user... %s with %d tokens"
					(user.User.name)
					(List.length active_tokens)
				);
				num_written := !num_written + 1;
				output#push (`Output (line^"\n"))
			in

			read_with_write_permission ~cast_read_err:write_error (fun proof users ->
				(* stream output while processing input *)
				let write_result = (fn write_user users |> LwtMonad.bind (function
					| Ok _ as result ->
						Log.debug (fun m->m "committing changes");
						return result
					| Error _ as err ->
						Log.debug (fun m->m "discarding changes");
						output#push `Rollback |> Lwt.map (fun () -> err)
				) |> LwtMonad.tap (fun _ ->
					Log.debug (fun m->m "closing output");
					return output#close
				)) in
				let write_io = Kv.write_s fs path ~proof output_chunks in
				lwt_join2 write_io write_result |> Lwt.map (fun (write_io, write_result) ->
					match write_io with
						| Ok () -> write_result
						| Error err -> Error (write_error err)
				)
			)
		)

		method modify_user : 'a 'err.
			string
			-> ?commit:('a option -> ('a, cancel) result)
			-> ((User.db_user -> unit Lwt.t) -> User.db_user -> ('a, 'err cancel_write) result Lwt.t)
			-> ('a, 'err cancel_write) result Lwt.t
		= fun username ?(commit=default_commit_policy) fn -> self#modify (fun write_user users ->
			Lwt_stream.fold_s (fun user result -> match (result, user) with
				| (Error _ as e, _) -> return e
				| (Ok _, Error (e:Kv.error)) -> return (Error (write_error e))
				| Ok result as acc, Ok user -> (
					if user.User.name = username then (
						match result with
						| Some _ ->
							Log.err (fun m->m "User %s appears more than once in DB" username);
							return (Error `Cancel)
						| None -> (
							fn write_user user |> Lwt.map (function
								| Ok x -> Ok (Some x)
								| Error _ as e -> e
							)
						)
					) else (write_user user |> Lwt.map (fun () -> acc))
				)
			) users (Ok None) |> Lwt.map (function
				| Ok result -> commit result |> R.reword_error (function `Cancel -> `Cancel)
				| Error _ as e -> e
			)
		)

		method read: 'a.
			((User.db_user, Kv.error) result Lwt_stream.t -> ('a, Kv.error) result Lwt.t)
			-> ('a, Kv.error) result Lwt.t
		= fun fn -> read_with_write_permission
			~cast_read_err:identity (fun _proof -> fn)

	end

	type signup_result = [
		| `Conflict
		| `Invalid_username
		| `Created of J.json
		]

	let signup ~(storage:storage) username password : (Token.sensitive_token, string) result Lwt.t = (
		storage#modify (fun write_user users ->
			let conflict = users |> Lwt_stream.find_s (function
				| Error _ -> return_true
				| Ok user ->
					if user.User.name = username
						then return_true
						else (write_user user |> Lwt.bindr (fun () -> return_false))
			) in

			conflict |> Lwt.bindr (function
				| Some (Ok (_:User.db_user)) -> return (Error (`Cancel))
				| Some (Error e) -> return (Error (write_error e))
				| None ->
					User.create ~clock:storage#clock ~username password |> Lwt_r.bind (fun (new_user, token) ->
						write_user new_user |> Lwt.map (fun () -> Ok token)
					) |> Lwt.map @@ R.reword_error rollback
			)
		) |> Lwt.map (R.reword_error (fun error ->
			let failed = "Account creation failed" in
			match error with
				| `Rollback `Invalid_username -> "Username must start with a letter and contain only letters, numbers dashes and underscores"
				| `Cancel -> failed
				| `Write_error e ->
						Log.warn (fun m->m "Unexpected error in account creation: %s" (Kv.string_of_error e));
						failed
		))
	)

	let get_user ~(storage:storage) username : (User.db_user option, Kv.error) result Lwt.t =
		storage#read (fun users ->
			users |> Lwt_stream.find (function
				| Ok user -> user.User.name = username
				| Error _ -> true (* terminate search *)
			) |> Lwt.map (function
				| Some (Ok user) -> Ok (Some user)
				| None -> Ok (None)
				| Some (Error _ as e) -> e
			)
		)

	let login ~(storage:storage) username password : (Token.sensitive_token, string) result Lwt.t =
		storage#modify_user username (fun write_user user ->
			match%lwt User.gen_token ~clock:storage#clock user password with
				| Some (user, token) -> write_user user |> Lwt.map (fun () -> Ok token)
				| None -> return (Error (`Rollback `Authentication_failed))
		) |> Lwt.map @@ R.reword_error (function
			| `Cancel | `Rollback `Authentication_failed -> "Authentication failed"
			| `Write_error e ->
				Log.warn (fun m->m "Unexpected error in login: %s" (Kv.string_of_error e));
				"Authentication error"
		)

	let token_has_expired ~(storage:storage) info =
		let expires = info.Token.expires in
		Ptime.is_earlier expires ~than:(time storage#clock)

	let validate ~(storage:storage) token : (User.db_user option, Kv.error) result Lwt.t =
		let info = token.sensitive_metadata in
		if token_has_expired ~storage info then
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

	let logout ~(storage:storage) token : (unit, Kv.error) result Lwt.t =
		let info = token.sensitive_metadata in
		if token_has_expired ~storage info then
			return (Ok ())
		else (
			let username = info.Token.user in
			storage#modify_user username (fun write_user user ->
				match User.remove_token user token with
					| Some user -> write_user user |> Lwt.map R.ok
					| None -> return (Error (`Rollback `Invalid_credentials))
			) |> Lwt.map (function
				| Error (`Write_error e) -> Error e
				| Ok () | Error `Cancel | Error (`Rollback `Invalid_credentials) -> Ok ()
			)
		)

	let change_password ~(storage:storage) user old new_password
		: (Token.sensitive_token option, Kv.error) result Lwt.t = (
		if User.verify (user.User.password) old then (
			storage#modify_user user.User.name (fun write_user db_user ->
				User.create ~clock:storage#clock ~username:db_user.User.name new_password
					(* |> Lwt.map (R.reword_error rollback) *)
					|> Lwt.bindr (function
						| Error e -> return (Error (`Rollback e))
						| Ok (new_user, token) -> (
							let open User in
							(* fields explicitly copied because we need to not overwrite any
							* future properties we add *)
							write_user {
								name=db_user.name;
								password=new_user.password;
								active_tokens=new_user.active_tokens;
							} >>= (fun () -> return (Ok token))
						)
					)
			) |> Lwt.map (function
				| Error (`Write_error e) -> Error e
				| Error (`Rollback `Invalid_username) | Error `Cancel -> Ok None
				| Ok x -> Ok (Some x)
			)
		) else (
			return (Ok None)
		)
	)

	let delete_user ~(storage:storage) user password : (bool, Kv.error) result Lwt.t =
		if User.verify (user.User.password) password then (
			storage#modify_user user.User.name (fun _write _user ->
				return (Ok true)
			) |> Lwt.map @@ R.reword_error (function
				| `Rollback _ | `Cancel -> assert false
				| `Write_error e -> e
			)
		) else (return (Ok false))

end
