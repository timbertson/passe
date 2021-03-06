open React_ext
open Lwt
open Common
module J = Json_ext

module Make (Server:Server.Sig)(Date:Date.Sig)(Re:Re_ext.Sig) = struct
	module Store = Store.Make(Re)
	module Auth = Client_auth.Make(Server)
	let db_url = Server.path ["db"]
	module Log = (val Logging.log_module "sync")

	type date = float
	type sync_state =
		| Uptodate
		| Updated_at of date
		| Syncing
		| Local_changes of int
	
	let string_of_sync_state = function
		| Uptodate -> "Uptodate"
		| Updated_at d -> "Updated_at " ^ (string_of_float d)
		| Syncing -> "Syncing"
		| Local_changes i -> "Local_changes " ^ (string_of_int i)

	let local_db_for_user config_provider uid =
		config_provider.Config.field ("db_"^uid)

	type initial_auth_state = [
		| `Explicit
		| Auth.online_implicit_auth_state
	]

	type env = {
		env_config_provider : Config.t;
		env_initial_auth: initial_auth_state
	}

	type state = {
		config_provider: Config.t;
		current_uid: Auth.username option signal;
		current_user_db: Config.child option signal;
		last_sync: Config.child;
		stored_json: J.json option signal;
		db_fallback: Store.t signal;
		db_signal: Store.t option signal;
		auth_state: Auth.auth_state signal;
		set_auth_state: Auth.auth_state -> unit;
		sync_running: bool signal;
		run_sync: Auth.authenticated_user_state -> (unit,string) result Lwt.t;
	}

	let sync_db (auth:Auth.authenticated_user_state) db =
		let send_db ~full db =
			let payload = if full then Store.to_json db else `Assoc [
				("changes", Store.Format.json_of_changes db.Store.changes);
				("version", `Int Store.(db.core.version));
			] in
			Server.post_json ~data:payload ?token:(Auth.token_of_authenticated auth) db_url
		in

		let%lwt response = send_db ~full:false db in
		match response with
			| Server.Failed (409, _, json) ->
				(* server may fail with { conflict:true, stored_version: int, <core: {...}> } *)
				let server_version = json |> Option.force |> J.mandatory J.int_field "stored_version" in
				let local_version = Store.(db.core.version) in
				Log.warn (fun m->m "Version conflict: local_version=%d, server_version=%d" local_version server_version);
				if local_version > server_version then (
					(* we have a newer version; just re-send the upload with the entire DB and let the server update itself *)
					send_db ~full:true db
				) else (
					(* if versions are equal we shouldn't have got a conflict. If
					* the server has a newer version than us, the server should have
					* resolved that internally *)
					failwith "version conflict reported, but I can't resolve it";
				)
			| response -> return response

	let _validate_implicit_auth () : Auth.online_implicit_auth_state Lwt.t =
		let%lwt auth = Server.call Auth.server_state_api J.empty in
		return (match Server.Response.force auth with
			| Some user -> `Implicit_user user
			| None -> `Anonymous
		)

	let _validate_explicit_auth = function
		| `Saved_user ((username, token) as credentials) ->
			let%lwt response = Server.call Auth.token_validate_api token in
			return (
				if Server.Response.force response then
					`Active_user credentials
				else
					`Failed_login username
			)

	let _validate_auth (auth:Auth.offline_auth_state) =
		let cast = fun a -> return (a:>Auth.auth_state) in
		match auth with
			| `Anonymous | `Saved_implicit_user _ -> _validate_implicit_auth () >>= cast
			| `Saved_user _ as auth -> _validate_explicit_auth auth >>= cast

	let _validate_auth_and_save validator t auth =
		let%lwt new_state = validator auth in
		t.set_auth_state (new_state:>Auth.auth_state);
		return new_state

	let validate_explicit_auth t (auth:[`Saved_user of Auth.credentials]) =
		_validate_auth_and_save _validate_explicit_auth t auth

	let validate_implicit_auth t =
		_validate_auth_and_save _validate_implicit_auth t ()

	let initial_auth_state auth_mode : initial_auth_state Lwt.t = (match auth_mode with
		| `Explicit ->
			Log.debug (fun m->m "initial_auth_state: using explicit auth");
			return `Explicit
		| `Implicit ->
			(* TODO: if we ever have an implicit auth state plus offline access, we should
			 * harmonize this a bit to store user details in DB, and to better represent the
			 * "you are offline and we have no idea if you're logged in" initial state *)
			Log.debug (fun m->m "initial_auth_state: fetching implicit state from server");
			let%lwt initial_state = try%lwt _validate_implicit_auth ()
			with e -> (
				Log.warn (fun m->m "Failed to load initial auth state: %s" (Printexc.to_string e));
				return `Anonymous
			) in
			Log.debug (fun m->m "initial_auth_state: initial state is %s"
				(Auth.string_of_auth_state (initial_state:>Auth.auth_state)));
			return (initial_state:>initial_auth_state)
	)

	let build env =
		let config_provider = env.env_config_provider in
		let field key:Config.child = config_provider.Config.field key in
		let last_sync = field "last_sync" in

		Log.debug (fun m->m "using server root: %s" (!Server.root_url |> Uri.to_string));

		let explicit_auth_signal () =
			let stored_credentials = field "credentials" in
			let stored_credentials_signal = stored_credentials#signal in
			let key_token = "token" in
			let key_user = "user" in
			let open Auth in
			let user_of_credentials stored =
				let explicit_state = match stored with
					| Some (`List [`String key; token]) when key = key_token ->
							`Saved_user (parse_credentials token)

					| Some (`List [`String key; `String username]) when key = key_user ->
							`Failed_login username

						(* backwards compat: assume a raw object is a token *)
					| Some (`Assoc _ as token) -> `Saved_user (parse_credentials token)

					| Some(j) ->
							Log.err (fun m->m "Failed to deserialize credentials");
							Log.debug (fun m->m "Credentials: %s" (J.to_string j));
							`Logged_out

					| None -> `Logged_out
				in
				(explicit_state:>auth_state)
			in
			let source = stored_credentials_signal
				|> S.map ~eq:never_eq user_of_credentials in
			let auth_state, _set_auth_state = editable_signal source in

			let set_auth_state (state:Auth.auth_state) =
				Log.debug (fun m->m "set_auth_state(%s)" (string_of_auth_state state));
				let step = Step.create () in
				begin match state with
				| `Logged_out -> (stored_credentials)#delete ~step ()
				| `Failed_login (username) ->
					stored_credentials#save ~step (`List [`String key_user; `String username])
				| `Saved_user (_, creds) | `Active_user (_, creds) ->
					stored_credentials#save ~step (`List [`String key_token; creds])
				| `Implicit_user _ | `Saved_implicit_user _ | `Anonymous -> failwith "set_auth_state called with implicit user type"
				end;
				_set_auth_state ~step state;
				Step.execute step
			in
			(auth_state, set_auth_state)
		in

		let implicit_auth_signal initial_auth =
			let (auth_state, set_auth_state) = S.create (initial_auth:>Auth.auth_state) in
			(auth_state, fun new_state -> set_auth_state new_state)
		in

		let auth_state, set_auth_state = match env.env_initial_auth with
			| `Explicit -> explicit_auth_signal ()

			| `Anonymous as u -> implicit_auth_signal u
			| `Implicit_user _ as u -> implicit_auth_signal u
		in

		let current_uid = auth_state |> S.map Auth.uid_of_state in

		let current_user_db : Config.child option signal =
			S.map (Option.map (local_db_for_user config_provider)) current_uid in

		let stored_json : J.json option signal = S.bind current_user_db (fun storage ->
			storage
				|> Option.map (fun s -> s#signal)
				|> Option.default (S.const None)
		) in

		let db_signal :Store.t option signal =
			stored_json |> S.map ~eq:(Option.eq Store.eq) (Option.map Store.parse_json)
		in

		let db_fallback :Store.t signal =
			db_signal |> S.map ~eq:Store.eq (Option.default Store.empty)
		in

		let set_last_sync_time t = last_sync#save (`Float t) in

		let sync_running, (run_sync:Auth.authenticated_user_state -> (unit,string) result Lwt.t) =
			let running_sync = ref None in
			let s, set_busy = S.create false in
			(s, fun auth ->
				match !running_sync with
					| Some (future,_) -> future
					| None -> begin
						let (_future, sync_complete) as task = Lwt.wait () in
						let finish result =
							running_sync := None;
							set_busy false;
							Lwt.wakeup sync_complete result;
							return result
						in

						running_sync := Some task;
						set_busy true;
						(
							try%lwt
								Log.info (fun m->m "syncing...");

								let uid = Auth.uid_of_authenticated auth in
								let db_storage = local_db_for_user config_provider uid in

								let get_latest_db () =
									db_storage#get |> Option.map Store.parse_json |> Option.default Store.empty in

								let sent_db = get_latest_db () in
								let%lwt response = sync_db auth sent_db in
								return (match response with
									| Server.OK json ->
										let open Store in
										let version = J.(mandatory int_field "version" json) in
										Log.info (fun m->m "server returned version %d" version);
										(* if the returned `version` is equal to the db we sent, then
										 * the server won't bother sending a payload, and we shouldn't bother
										 * trying to process it *)
										if version <> sent_db.core.version then begin
											assert (version > sent_db.core.version);
											let new_core = Store.Format.core_of_json json in
											let new_db = Store.drop_applied_changes
												~from:(get_latest_db ())
												~new_core sent_db.Store.changes in
											db_storage#save (Store.to_json new_db);
										end;
										set_last_sync_time (Date.time ());
										Ok ()
									| Server.Unauthorized msg ->
										set_auth_state (Auth.failed_login_of_authenticated auth);
										Error (Printf.sprintf
											"authentication failed: %a"
											(Option.print print_string) msg)
									| Server.Failed (_, msg, _) ->
										set_auth_state (Auth.saved_user_of_authenticated auth);
										Error msg
								)
							with e -> return (Error (
								"uncaught error in run_sync: " ^ (Printexc.to_string e)
							))
						) >>= finish
				end
			)
		in

		{
			current_uid=current_uid;
			current_user_db=current_user_db;
			stored_json=stored_json;
			db_signal=db_signal;
			db_fallback=db_fallback;
			last_sync=last_sync;
			auth_state=auth_state;
			set_auth_state=set_auth_state;
			config_provider=config_provider;
			sync_running=sync_running;
			run_sync=run_sync;
		}

	let login ~user ~password t =
		let%lwt response = Server.call Auth.login_api (Auth.payload ~user ~password) in
		let open Server in
		let result = begin match response with
			| OK creds ->
				(`Active_user creds)
			| Failed (_, message, _) ->
					Log.warn (fun m->m "Failed login: %s" message);
					`Failed_login user
			| Unauthorized _ -> assert false
		end in
		t.set_auth_state result;
		return result

	let _mutate state fn =
		match S.value state.current_user_db with
			| None -> Log.err (fun m->m "Can't alter DB - no current user"); false
			| Some user_db ->
				let current_db = S.value state.db_fallback in
				let new_db = fn current_db in
				Log.info (fun m->m "Saving new DB: %s" (Store.to_json_string new_db));
				user_db#save (Store.to_json new_db);
				true

	let save_change ~state ~original updated =
		_mutate state (fun db -> Store.update ~db ~original updated)

	let save_default ~state change =
		_mutate state (fun db ->
			Store.({db with changes = db.changes @ [Default change]})
		)
end
