open Common
open React_ext
open Ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest

let log = Logging.get_logger "sync"

let username_key = "user"
let login_url = Server.path ["auth"; "login"]
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
	set_auth_state: bool -> auth_state -> unit;
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

		let set_auth_state remember_me state =
			let step = Step.create () in
			begin match state with
			| Anonymous -> (stored_credentials)#delete ~step ()
			(* TODO: Failed_login & Saved_user *)
			| Active_user (_, creds) ->
					if remember_me then
						(stored_credentials)#save ~step creds
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
	}


let ui state =
	(* let config_provider, set_config_provider = S.create (Lazy.force ephemeral_config) in *)
	let set_last_sync_time t = (state.last_sync)#save (`Float t) in
	let last_sync_signal = state.last_sync#signal in
	let last_sync_time = last_sync_signal |> signal_lift_opt (function
		| `Float t -> t
		| _ -> raise @@ AssertionError ("invalid `last_sync` value")
	) in

	let remember_me_input = input ~attrs:[("type","checkbox");("checked","true")] () in
	let remember_me = signal_of_checkbox ~initial:true remember_me_input in
	let set_auth_state = state.set_auth_state (S.value remember_me) in

	let sync_running, (run_sync:credentials -> unit Lwt.t) =
		let running_sync = ref None in
		let s, set_busy = S.create false in
		(s, fun (username, token) ->
			match !running_sync with
				| Some (future,_) -> future
				| None -> begin
					let (future, sync_complete) as task = Lwt.wait () in
					running_sync := Some task;
					set_busy true;
					try_lwt
						log#info "syncing...";
						let db_storage = local_db_for_user
							state.config_provider
							username in

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
								log#error "sync failed: %s" msg
						);
						return_unit;
					with e -> (
						Lwt.wakeup_exn sync_complete e;
						raise e
					)
					finally (
						running_sync := None;
						set_busy false;
						Lwt.wakeup sync_complete ();
						return_unit
					)
			end
		) in

	let sync_state = S.bind sync_running (fun is_running ->
		if is_running then S.const Syncing else (
			S.l2 (fun db sync_time ->
				let len = List.length db.Store.changes in
				if len = 0 then
					sync_time |> Option.map (fun t -> Updated_at t) |> Option.default Uptodate
				else Local_changes len
			) state.db_signal last_sync_time
		)) in


	let login_form (username:string option) =
		let error, set_error = S.create None in

		let error_widget = error
			|> optional_signal_content (fun err -> Ui.p ~cls:"text-danger" ~text:err ~children:[
				child i ~cls:"glyphicon glyphicon-remove" ();
			] ())
			|> Ui.stream in
		
		div ~cls:"account-status login alert alert-info" ~children:[
			child form ~cls:"login form-inline" ~attrs:[("role","form")] ~children:[
				error_widget;
				child input ~cls:"btn btn-primary" ~attrs:[("type","submit"); ("value","Sign in")] ();
				child div ~cls:"form-group form-group-sm email" ~children:[
					child label ~cls:"sr-only" ~text:"User" ();
					child input ~cls:"form-control" ~attrs:[
						("name","user");
						("placeholder","User");
						("type","text");
						("value", Option.default "" username);
					] ();
				] ();
				space;

				child div ~cls:"form-group form-group-sm password" ~children:[
					child label ~cls:"sr-only" ~text:"password" ();
					child input ~cls:"form-control" ~attrs:[
						("type","password");
						("name","password");
						("placeholder","password")
					] ();
				] ();

				space;

				child div ~cls:"checkbox remember-me form-group" ~children:[
					frag remember_me_input;
					space;
					child label ~text:"Remember me" ();
				] ();

				child div ~cls:"clearfix" ();
			] ~mechanism:(fun elem ->
				effectful_stream_mechanism (remember_me
					|> S.map (fun remember_me ->
							log#info "remember me changed to: %b" remember_me;
							if (not remember_me) then (
								(state.stored_credentials)#delete ()
							)
					)
				)
				<&>
				Lwt_js_events.submits elem (fun event _ ->
					stop event;
					log#info "form submitted";
					let data = `Assoc (Form.get_form_contents elem |> List.map (fun (name, value) -> (name, `String value))) in
					let open Server in
					set_error None;
					lwt response = post_json ~data login_url in
					begin match response with
					| OK response ->
						let creds = J.get_field "token" response in
						let username = creds |> Option.bind (J.string_field username_key) in
						(match (username, creds) with
							| Some user, Some creds -> set_auth_state (Active_user (user, creds))
							| _ -> raise (AssertionError "credentials doesn't contain a user key")
						)
					| Failed (message, _) -> set_error (Some message)
					| Unauthorized _ -> assert false
					end;
					return_unit
				)
			) ()
		] ()
	in

	let sync_state_widget auth =
		let open Ui in
		let _node w = (w:>Dom.node widget_t) in
		let sync_mech = (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				run_sync auth
			)
		) in
		sync_state |> S.map (function
			| Uptodate -> _node @@ empty ()
			| Updated_at date ->
					let now = Date.time () in
					let time_diff = max 0.0 (now -. date) in
					_node @@ a ~cls:"link has-tooltip"
						~attrs:[("title", "Last sync " ^ (Date.human_time_span_desc time_diff) ^ " ago")]
						~children:[icon "refresh"]
						~mechanism:sync_mech ()
			| Syncing ->
				_node @@ span
					~cls:"syncing"
					~children:[icon "refresh"] ()
			| Local_changes count ->
					_node @@ span
						~attrs:[("title", (string_of_int count) ^ " pending changes...")]
						~children:[
					child a
						~cls:"link"
						~children:[icon "upload"]
						~mechanism:sync_mech ()
				] ()
		) |> stream
	in

	let logout_button tooltip creds = child a
		~cls:"hover-reveal link"
		~attrs:["title",tooltip]
		~children:[icon "remove"]
		~mechanism:(fun elem ->
			Lwt_js_events.clicks elem (fun evt _ ->
				let open Server in
				match_lwt post_json ~data:creds logout_url with
					| OK _ | Unauthorized _ ->
						set_auth_state Anonymous;
						return_unit;
					| Failed (msg,_) ->
						log#error "Can't log out: %s" msg;
						return_unit
			)
		) ()
	in

	state.auth_state |> S.map (fun auth ->
	log#info "Auth state: %s" (string_of_auth_state auth);

	match auth with
	| Anonymous -> login_form None
	| Failed_login username -> login_form (Some username)
	| Saved_user (username, creds) -> (
		let busy, set_busy = S.create true in

		let offline_message = span ~text:"offline " () in
		offline_message#class_s "hidden" busy;

		let sync_button = a ~children:[icon "refresh"] () in
		sync_button#class_s "syncing" busy;
		sync_button#class_s "link" (S.map not busy);

		div ~cls:"account-status alert alert-warning" ~children:[
			child span ~cls:"user" ~text:username ();
			logout_button "Log out" creds;
			child span ~cls:"offline" ~children:[
				frag offline_message;
				frag sync_button;
			] ~mechanism:(fun elem ->
				let continue = ref true in
				while_lwt !continue do
					set_busy true;
					continue := false;
					let open Server in
					match_lwt Server.post_json ~data:creds token_validate_url with
					| OK _ -> set_auth_state (Active_user (username, creds)); return_unit
					| Unauthorized msg ->
						log#warn "failed auth: %a" (Option.print output_string) msg;
						set_auth_state (Failed_login username);
						return_unit
					| Failed (msg, _) ->
						log#warn "unknown failure, assuming connectivity issue: %s" msg;
						continue := true;
						set_busy false;
						Lwt.pick [
							(
								lwt (_:Dom_html.mouseEvent Js.t) = Lwt_js_events.click elem in
								return_unit
							);
							(Lwt_js.sleep 60.0);
						]
				done
			) ()
		] ()
	)
	| Active_user ((username, creds) as auth) -> (
		div ~cls:"account-status alert alert-success" ~children:[
			child span ~cls:"user" ~text:username ();
			logout_button "Log out" creds;
			child span ~cls:"online" ~children:[
				sync_state_widget auth;
			] ()
		]
		~mechanism:(fun elem ->
			let run_sync () = run_sync auth in
			lwt () = run_sync () in
			while_lwt true do
				log#info "sync loop running..";
				Lwt.pick [
					(* every 2 minutes, attempt a sync *)
					(Lwt_js.sleep 120.0 >>= run_sync);
					(effectful_stream_mechanism (sync_state |> S.map (function
						| Local_changes _ -> Lwt_js.sleep 2.0 >>= run_sync
						| _ -> return_unit
					)));
				]
			done
		) ()
	)
) |> Ui.stream
