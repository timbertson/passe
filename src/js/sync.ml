open Common
open React_ext
open Ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest

let log = Logging.get_logger "sync"

let stored_credentials = Config.field "credentials"
let last_sync = Config.field "last_sync"
let set_last_sync_time t = last_sync#save (`Float t)
let last_sync_time = last_sync#signal |> signal_lift_opt (function
	| `Float t -> t
	| _ -> raise @@ AssertionError ("invalid `last_sync` value")
)


let username_key = "user"
let login_url = Server.path ["auth"; "login"]
let token_validate_url = Server.path ["auth"; "validate"]
let db_url username = Server.path ["db"; username]

type username = string
type credentials = string * J.json
type auth_state =
	| Anonymous
	| Expired_user of username
	| Saved_user of credentials
	| Active_user of credentials

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

let auth_state, _set_auth_state =
	let initial = stored_credentials#get
		|> Option.map (fun token -> Saved_user (parse_credentials token))
		|> Option.default Anonymous
	in
	S.create initial

let current_username = auth_state |> S.map (function
	| Anonymous -> None
	| Expired_user u | Saved_user (u, _) | Active_user (u, _) -> Some u
)

let local_db_for_user user = Config.field ("db_" ^ user)
let current_user_db : Config.child option signal =
	current_username |> signal_lift_opt local_db_for_user

let stored_json : J.json option signal = S.bind current_user_db (fun storage ->
	storage
		|> Option.map (fun s -> s#signal)
		|> Option.default (S.const None)
)

let db_signal :Store.t signal =
	stored_json |> S.map ~eq:Store.eq (fun json ->
		json
		|> Option.map Store.parse_json
		|> Option.default Store.empty
)

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
					let db_storage = local_db_for_user username in
					let get_latest_db () =
						db_storage#get |> Option.map Store.parse_json |> Option.default Store.empty in

					let sent_changes = (get_latest_db ()).Store.changes in

					lwt response = (match sent_changes with
						| [] -> Server.get_json (db_url username)
						| sent_changes ->
								let data = (Store.Format.json_of_changes sent_changes) in
								Server.post_json ~data (db_url username)
					) in
					(match response with
						| Server.OK json ->
								let new_core = Store.Format.core_of_json json in
								let new_db = Store.drop_applied_changes
									~from:(get_latest_db ())
									~new_core sent_changes in
								db_storage#save (Store.to_json new_db);
								set_last_sync_time (Date.time ());
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
	)

let sync_state = S.bind sync_running (fun is_running ->
	if is_running then S.const Syncing else (
		S.l2 (fun db sync_time ->
			let len = List.length db.Store.changes in
			if len = 0 then
				sync_time |> Option.map (fun t -> Updated_at t) |> Option.default Uptodate
			else Local_changes len
		) db_signal last_sync_time
	))


let remember_me_input = input ~attrs:[("type","checkbox");("checked","true")] ()
let remember_me = signal_of_checkbox ~initial:true remember_me_input

(* wrap set_auth_state aith automatic updating of localStorage *)
let set_auth_state state = begin match state with
	| Anonymous -> stored_credentials#delete
	(* TODO: Expired_user & Saved_user *)
	| Active_user (_, creds) ->
			if S.value remember_me then
				stored_credentials#save creds
	| _ -> ()
	end;
	_set_auth_state state

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
							stored_credentials#delete
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
				| OK creds ->
					let username = creds
						|> J.get_field username_key
						|> Option.bind J.as_string in
					(match username with
						| Some user -> set_auth_state (Active_user (user, creds))
						| None -> raise (AssertionError "credentials doesn't contain a user key")
					)

				| Failed (message, _) -> set_error (Some message)
				end;
				return_unit
			)
		) ()
	] ()

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

let ui () =
	auth_state |> S.map (fun auth -> match auth with
	| Anonymous -> login_form None
	| Expired_user username -> login_form (Some username)
	| Saved_user (username, creds) -> (
		let offline_message, set_offline_message = S.create
			(span ~text:"connecting..." ()) in

		div ~cls:"account-status alert alert-warning" ~children:[
			child span ~cls:"user" ~text:username ();
			child span ~cls:"offline" ~children:[
				stream offline_message;
			] ~mechanism:(fun offline ->
				let continue = ref true in
				while_lwt !continue do
					continue := false;
					let open Server in
					match_lwt Server.post_json ~data:creds token_validate_url with
					| OK _ -> set_auth_state (Active_user (username, creds)); return_unit
					| Failed (msg, json) ->
						log#warn "failed auth: %s" msg;
						let reason = json |> Option.bind (J.get_field "reason") |> Option.bind J.as_string in
						match reason with
							| Some reason ->
									log#warn "credentials rejected: %s" reason;
									set_auth_state Anonymous;
									stored_credentials#delete;
									return_unit
							| None ->
								continue := true;
								set_offline_message (
									span ~children:[
										child i ~cls:"glyphicon glyphicon-remove" ();
										child span ~text:"offline" ();
									] ());
								log#info "no reason given for rejection; assuming connectivity issue";
								Ui.withContent offline ?before:(offline##firstChild |> Js.Opt.to_option) (a ~cls:"link retry disabled" ~children:[
									child i ~cls:"glyphicon glyphicon-refresh" ();
									child span ~text:"retry" ();
								] ()) (fun elem ->
									pick [
										(
											lwt (_:Dom_html.mouseEvent Js.t) = Lwt_js_events.click elem in
											return_unit
										);
										(Lwt_js.sleep 60.0);
									]
								)
				done
			) ()
		] ()
	)
	| Active_user ((username, _) as auth) -> (
		div ~cls:"account-status alert alert-success" ~children:[
			child span ~cls:"user" ~text:username ();
			child a ~cls:"hover-visible btn btn-default" ~text:"Log out"
			~mechanism:(fun elem ->
				Lwt_js_events.clicks elem (fun evt _ ->
					set_auth_state Anonymous;
					return_unit;
				)
			) ();
			child span ~cls:"online" ~children:[
				sync_state_widget auth;
			] ()
		]
		~mechanism:(fun elem ->
			let run_sync () = run_sync auth in
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
