open Common
open React_ext
open Lwt
module J = Json_ext

let log = Logging.get_logger "sync"

let username_key = "user"
let login_url = Server.path ["auth"; "login"]
let signup_url = Server.path ["auth"; "signup"]
let logout_url = Server.path ["auth"; "logout"]
let token_validate_url = Server.path ["auth"; "validate"]
let db_url = Server.path ["db"]

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


type date = float
type sync_state =
	| Uptodate
	| Updated_at of date
	| Syncing
	| Local_changes of int

let parse_credentials (token:J.json) : credentials =
	let user = token
		|> J.get_field username_key
		|> Option.bind (J.as_string)
		|> Option.default_fn (fun () ->
				raise (AssertionError ("no username found in auth token")))
	in (user, token)

let local_db_for_user config_provider user =
	config_provider.Config.field ("db_"^user)

type state = {
	config_provider: Config.t;
	current_username: username option signal;
	current_user_db: Config.child option signal;
	last_sync: Config.child;
	stored_json: J.json option signal;
	db_signal: Store.t signal;
	stored_credentials: Config.child;
	stored_credentials_signal: J.json option signal;
	auth_state: auth_state signal;
	set_auth_state: auth_state -> unit;
	sync_running: bool signal;
	run_sync: credentials -> unit Lwt.t;
}

let build config_provider =
	let field key:Config.child = config_provider.Config.field key in
	let stored_credentials = field "credentials" in
	let stored_credentials_signal = stored_credentials#signal in
	let last_sync = field "last_sync" in

	let auth_state, set_auth_state =
		let source = stored_credentials_signal |> S.map ~eq:never_eq
			(fun credentials ->
				credentials
				|> Option.map (fun token -> Saved_user (parse_credentials token))
				|> Option.default Anonymous
			) in
		let auth_state, _set_auth_state = editable_signal source in

		let set_auth_state state =
			let step = Step.create () in
			begin match state with
			| Anonymous -> (stored_credentials)#delete ~step ()
			(* TODO: Failed_login & Saved_user *)
			| Active_user (_, creds) -> stored_credentials#save ~step creds
			| _ -> ()
			end;
			_set_auth_state ~step state;
			Step.execute step
		in
		(auth_state, set_auth_state)
	in

	let current_username = auth_state |> S.map (function
		| Anonymous -> None
		| Failed_login u | Saved_user (u, _) | Active_user (u, _) -> Some u
	) in

	let current_user_db : Config.child option signal =
		S.map (Option.map (local_db_for_user config_provider)) current_username in

	let stored_json : J.json option signal = S.bind current_user_db (fun storage ->
		storage
			|> Option.map (fun s -> s#signal)
			|> Option.default (S.const None)
	) in

	let db_signal :Store.t signal =
		stored_json |> S.map ~eq:Store.eq (fun json ->
			json
			|> Option.map Store.parse_json
			|> Option.default Store.empty
	) in

	let set_last_sync_time t = last_sync#save (`Float t) in

	let sync_running, (run_sync:credentials -> unit Lwt.t) =
		let running_sync = ref None in
		let s, set_busy = S.create false in
		(s, fun credentials ->
			let (username, token) = credentials in
			match !running_sync with
				| Some (future,_) -> future
				| None -> begin
					let (future, sync_complete) as task = Lwt.wait () in
					let finish err =
						running_sync := None;
						set_busy false;
						match err with
							| Some e -> Lwt.wakeup_exn sync_complete e; raise e
							| None   -> Lwt.wakeup sync_complete (); return_unit
					in

					running_sync := Some task;
					set_busy true;
					try_lwt
						log#info "syncing...";
						let db_storage = local_db_for_user config_provider username in

						let get_latest_db () =
							db_storage#get |> Option.map Store.parse_json |> Option.default Store.empty in

						let sent_changes = (get_latest_db ()).Store.changes in

						lwt response = (match sent_changes with
							| [] -> Server.get_json ~token db_url
							| sent_changes ->
									let data = Store.Format.json_of_changes sent_changes in
									Server.post_json ~data ~token (db_url)
						) in
						(match response with
							| Server.OK json ->
									let new_core = Store.Format.core_of_json json in
									let new_db = Store.drop_applied_changes
										~from:(get_latest_db ())
										~new_core sent_changes in
									db_storage#save (Store.to_json new_db);
									set_last_sync_time (Date.time ());
							| Server.Unauthorized msg ->
								log#error "authentication failed: %a" (Option.print output_string) msg;
								set_auth_state (Failed_login username)
							| Server.Failed (msg, _) ->
								log#error "sync failed: %s" msg;
								set_auth_state (Saved_user credentials)
						);
						finish None
					with e -> finish (Some e)
			end
		)
	in

	{
		current_username=current_username;
		current_user_db=current_user_db;
		stored_json=stored_json;
		db_signal=db_signal;
		last_sync=last_sync;
		stored_credentials=stored_credentials;
		stored_credentials_signal=stored_credentials_signal;
		auth_state=auth_state;
		set_auth_state=set_auth_state;
		config_provider=config_provider;
		sync_running=sync_running;
		run_sync=run_sync;
	}


