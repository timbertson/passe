open Passe
open Passe_js
open Common
open React_ext
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest
open Sync

type error_message = string
type login_form = {
	username: string option;
	password: string option;
}
type ui_state = {
	error: error_message option;
	busy: bool;
	login_form: login_form;
}

type state = {
	auth_state: Client_auth.auth_state;
	sync_state: Sync.sync_state;
	ui: ui_state;
}

let string_of_login_form = function { username; password } ->
	"{ username = " ^ (Option.to_string quote_string username) ^
	"; password = " ^ (Option.to_string mask_string password) ^
	" }"

let string_of_ui_state = function { error; busy; login_form } ->
	"{ error = " ^ (Option.to_string quote_string error) ^
	"; busy = " ^ (string_of_bool busy) ^
	"; login_form = " ^ (string_of_login_form login_form) ^
	" }"

let string_of_state = function { auth_state; sync_state; ui } ->
	"{ auth_state = " ^ (Client_auth.string_of_auth_state auth_state) ^
	"; sync_state = " ^ (Sync.string_of_sync_state sync_state) ^
	"; ui = " ^ (string_of_ui_state ui) ^
	" }"

type field = [
	| `username of string
	| `passe_password of string
]

type internal_ui_message = [
	| `error of error_message option
	| `busy of bool
	| `request_sync
	| `request_logout of J.json
	| `request_login
	| `request_signup
	| `edit of field
]

type ui_message = [
	| internal_ui_message
	| `show_account_settings
]

type external_state_notification = [
	| `auth_state of Client_auth.auth_state
	| `sync_state of Sync.sync_state
]

type internal_message = [
	| internal_ui_message
	| external_state_notification
]

type message = [
	| ui_message
	| external_state_notification
]

let string_of_field : field -> string = function
	| `username u -> "username " ^ u
	| `passe_password _ -> "password ( ... )"

let string_of_message : ([<message]) -> string = function
	| `auth_state s -> "`auth_state " ^ (Client_auth.string_of_auth_state s)
	| `sync_state s -> "`sync_state " ^ (Sync.string_of_sync_state s)
	| `error err -> "`error " ^ (Option.to_string quote_string err)
	| `busy b -> "`busy " ^ (string_of_bool b)
	| `request_sync -> "`request_sync"
	| `request_logout _ -> "`request_logout"
	| `request_login -> "`request_login"
	| `request_signup -> "`request_signup"
	| `show_account_settings -> "`show_account_settings"
	| `edit field -> "`edit (" ^ (string_of_field field) ^ ")"

let login_form_of_auth_state current: Client_auth.auth_state -> login_form = function
	| `Logged_out | `Anonymous
		-> { username = None; password = None }

	| `Failed_login u
	| `Saved_user (u, _)
	| `Saved_implicit_user (u, _)
		-> { current with username = Some u }

	| `Active_user _
	| `Implicit_user _
		-> current

(* === Begin view functions === *)

open Vdoml

let events_of_signal s =
	let e, emit = E.create () in
	emit (S.value s);
	E.select [e; S.changes s]

let sync_state sync = 
	let last_sync_signal = sync.last_sync#signal in
	let last_sync_time = last_sync_signal |> signal_lift_opt (function
		| `Float t -> t
		| _ -> raise @@ AssertionError ("invalid `last_sync` value")
	) in
	let sync_running = sync.sync_running in
	S.bind sync_running (fun is_running ->
		if is_running then S.const Syncing else (
			S.l2 (fun db sync_time ->
				let len = List.length db.Store.changes in
				if len = 0 then
					sync_time |> Option.map (fun t -> Updated_at t) |> Option.default Uptodate
				else Local_changes len
			) sync.db_fallback last_sync_time
		)
	)

let logout ~set_auth_state = fun token -> (
	let open Server in
	match_lwt post_json ~data:token Client_auth.logout_url with
		| OK _ | Unauthorized _ ->
			set_auth_state `Logged_out;
			return_unit;
		| Failed (_, msg,_) ->
			Log.err (fun m->m "Can't log out: %s" msg);
			return_unit
)

let submit_form ~set_auth_state url instance {username; password} = (
	Log.info (fun m->m "form submitted");
	let data = match username, password with
		| Some username, Some password ->
			Some (`Assoc [
				"user", `String username;
				"password", `String password;
			])
		| _ ->
			Ui.emit instance (`error (Some "Username and password required"));
			None
	in
	data |> Option.map (fun data ->
		Ui.emit instance (`error None);
		let open Server in
		Server.post_json ~data url |> Lwt.map (function
			| OK response ->
				set_auth_state (`Active_user (Auth.get_response_credentials response));
				None
			| Failed (_, message, _) ->
				Some (`error (Some message))
			| Unauthorized _ -> assert false
		)
	) |> Option.default (Lwt.return None)
)

let auth_loop ~set_auth_state instance (auth:Client_auth.saved_auth_state) = (
	let continue = ref true in
	let emit = Ui.emit instance in
	while_lwt !continue do
		emit (`busy true);
		try_lwt (
			continue := false;
			let open Server in
			let response = match auth with
				| `Saved_user (_username, creds) ->
					Server.post_json ~data:creds Client_auth.token_validate_url
				| `Saved_implicit_user _ ->
					Server.post_json ~data:(`Assoc []) Client_auth.server_state_url
			in

			match_lwt response with
			| OK json -> return (match auth with
				| `Saved_user u -> set_auth_state (`Active_user u)
				| `Saved_implicit_user _ ->
					set_auth_state (match Client_auth.parse_implicit_user json with
						| Some u -> `Implicit_user u
						| None -> `Anonymous
					)
				)
			| Unauthorized msg ->
				Log.warn (fun m->m "failed auth: %a" (Option.fmt Format.pp_print_string) msg);
				let auth = (auth:>Client_auth.authenticated_user_state) in
				set_auth_state (Auth.failed_login_of_authenticated auth);
				return_unit
			| Failed (_, msg, _) ->
				Log.warn (fun m->m "unknown failure: %s" msg);
				continue := true;
				emit (`busy false);
				Lwt_js.sleep 60.0
		) finally (
			emit (`busy false);
			Lwt.return_unit
		)
	done
)

let sync_db_loop ~sync ~sync_state auth =
	(* used when signed in, to periodically sync DB state *)
	let run_sync auth =
		try_lwt
			sync.run_sync auth
		with e -> begin
			Log.err (fun m->m "%s" (Printexc.to_string e));
			(* XXX todo: remove this when we track down the error *)
			raise e
		end
	in

	while_lwt true do
		Log.info (fun m->m "sync loop running..");
		lwt () = run_sync (auth:>Client_auth.authenticated_user_state) in
		let next_change = (sync_state
			|> events_of_signal
			|> E.filter (function | Local_changes _ -> true | _ -> false)
			|> Lwt_react.E.next)
		in
		Lwt.pick [
			(* every 30 minutes *)
			(Lwt_js.sleep 18000.0);
			(* shortly after making a change to the DB *)
			(next_change >>= (fun _ -> Lwt_js.sleep 2.0))
		]
	done

let run_background_sync ~sync ~set_auth_state instance sync_state : (Client_auth.auth_state option -> message option Lwt.t) =
	let last_auth = ref None in
	let run = (fun auth ->
		last_auth := Some auth;
		Log.info (fun m->m "run_background_sync called");
		(match auth with
			| `Logged_out | `Anonymous | `Failed_login _ -> return_unit
			| `Saved_user _ | `Saved_implicit_user _ as u ->
				auth_loop ~set_auth_state instance (u:>Client_auth.saved_auth_state)
			| `Active_user _ | `Implicit_user _ as u ->
				sync_db_loop ~sync ~sync_state (u:>Client_auth.logged_in_user_state)
		) >>= fun _ -> return_none
	) in
	fun auth -> auth
		|> Option.or_ !last_auth
		|> Option.map run
		|> Option.default Lwt.return_none

let command sync instance : (state, message) Ui.command_fn  = (
	let set_auth_state = sync.Sync.set_auth_state in
	let logout = Ui.supplantable (fun token ->
		logout ~set_auth_state token |> Lwt.map (fun _ -> None)
	) instance in

	let login = Ui.supplantable (fun login_form ->
		submit_form ~set_auth_state Client_auth.login_url instance login_form
	) instance in

	let signup = Ui.supplantable (fun login_form ->
		submit_form ~set_auth_state Client_auth.signup_url instance login_form
	) instance in

	let sync_state = sync_state sync in
	let background_sync = Ui.supplantable
		(run_background_sync ~sync ~set_auth_state instance sync_state) instance in

	(* kick off initial background syc *)
	background_sync (Some (S.value sync.Sync.auth_state)) |> Ui.async instance;
	Passe_ui.emit_changes instance sync_state (fun s -> `sync_state s);

	fun state message -> match message with
		| `request_logout token -> Some (logout token)
		| `request_login -> Some (login state.ui.login_form)
		| `request_signup -> Some (signup state.ui.login_form)
		| `auth_state auth -> Some (background_sync (Some auth))
		| `request_sync -> Some (background_sync None)
		| _ -> None
)

let update state (message:internal_message) =
	let update_ui state message = match message with
		| `error error -> { state with error }
		| `busy busy -> { state with busy }
		| `edit field -> (match field with
			| `username u -> { state with login_form = { state.login_form with username = Some u } }
			| `passe_password p -> { state with login_form = { state.login_form with password = Some p } }
		)
	in
	match message with
	| `auth_state auth_state ->
		{ state with
			auth_state;
			ui = { state.ui with
				login_form = login_form_of_auth_state state.ui.login_form auth_state;
			};
		}
	| `sync_state sync_state -> { state with sync_state }
	| `error _ | `busy _ | `edit _ as message -> { state with ui = update_ui state.ui message }

	(* used only for `command` side effects *)
	| `request_sync | `request_logout _ | `request_login | `request_signup
		-> state

let initial_state sync = {
	ui = {
		error = None;
		busy = false;
		login_form = {
			username = None;
			password = None;
		};
	};
	auth_state = sync.Sync.auth_state |> S.value;
	sync_state = sync_state sync |> S.value;
}

type sync_state = {
	sync_error : string option;
	sync_auth : Client_auth.auth_state;
}



open Html
open Bootstrap

let view_sync_state = (function
	| Uptodate -> text "";
	| Updated_at date ->
			let now = Date.time () in
			let time_diff = max 0.0 (now -. date) in
			a ~a:[
				a_onclick (emitter `request_sync);
				a_class "link has-tooltip";
				(* TODO: relative time will never actually be updated... Need to put it in state? *)
				a_title ("Last sync " ^ (Date.human_time_span_desc time_diff) ^ " ago");
			] [icon "refresh"]
	| Syncing ->
		span ~a:[a_class "syncing"] [icon "refresh"]
	| Local_changes count ->
		span ~a:[a_title ((string_of_int count) ^ " pending changes...")] [
			a ~a:[a_class "link"; a_onclick (emitter `request_sync)] [icon "upload"]
		]
	)

let emit_on_return action : message attr = a_onkeydown (handler (fun evt ->
	evt
		|> Event.keyboard_event
		|> Option.map (fun evt -> evt##keyCode)
		|> Option.filter ((=) Keycode.return)
		|> Option.map (fun _ -> Event.handle action)
		|> Event.optional
))

let view_login_form _instance =
	let submit_on_return = emit_on_return `request_login in
	let track_username = track_input_contents (fun text -> `edit (`username text)) in
	let track_password = track_input_contents (fun text -> `edit (`passe_password text)) in

	fun stored_username {error; login_form=form; _} -> (
		let space = text " " in
		let error_text = error |> Option.map (fun err ->
			p ~a:[a_class "text-danger"] [
				Bootstrap.icon "remove";
				text err;
			]
		) |> Option.to_list in

		div ~a:[a_class "account-status login alert alert-info"] [
			div ~a:[a_class "login form-inline"; a_role "form"] (error_text @ [
				div ~a:[a_class "form-group form-group-xs email"] [
					label ~a:[a_class "sr-only"] [text "User"];
					input ~a:[
						a_class "form-control username-input";
						a_name "user";
						a_placeholder "User";
						a_input_type `Text;
						a_value (Option.default "" (form.username |> Option.or_ stored_username));
						a_oninput track_username;
						submit_on_return;
					] ();
				];
				space;

				div ~a:[a_class "form-group form-group-xs password"] [
					label ~a:[a_class "sr-only"] [text "password"];
					input ~a:[
						a_class "form-control password-input";
						a_input_type `Password;
						a_name "password";
						a_placeholder "password";
						a_value (Option.default "" form.password);
						a_oninput track_password;
						submit_on_return;
					] ();
				];

				space;
				input ~a:[
					a_class "btn btn-primary login";
					a_input_type `Submit;
					a_value "Sign in";
					a_onclick (emitter `request_login);
				] ();
				input ~a:[
					a_class "btn btn-default muted signup pull-right";
					a_input_type `Button;
					a_value "Register";
					a_onclick (emitter `request_signup);
				] ();

				div ~a:[a_class "clearfix"] [];
			])
		];
	)

let optional_logout_ui (auth:Client_auth.authenticated_user_state) = (
	Client_auth.token_of_authenticated auth |> Option.map (fun token ->
		a ~a:[
			a_class "hover-reveal link";
			a_title "Log out";
			a_onclick (emitter (`request_logout token));
		] [icon "remove"]
	) |> Option.to_list
)

let view_logged_in_user instance =
	let account_settings_button = Ui.child
		~message:(fun () -> `show_account_settings)
		Account_settings.button
		instance
	in
	fun (auth:Client_auth.authenticated_user_state) sync_state ->
		let username = Client_auth.name_of_authenticated auth in
		div ~a:[a_class "account-status alert alert-success"] ([
			span ~a:[a_class "user"] [text username];
		] @ (optional_logout_ui auth) @ [
			span ~a:[a_class "online"] [
				account_settings_button ();
				view_sync_state sync_state;
			];
		]
	)

let view_saved_user _instance = fun (auth: Auth.saved_auth_state) {busy; _} -> (
	let username = Auth.name_of_saved auth in
	let offline_message = span
		~a:[if busy then a_class "hidden" else None]
		[text "offline "] in

	let sync_button = a
		~a:[
			a_class (if busy then "syncing" else "link");
			a_onclick (emitter `request_sync);
		] [Bootstrap.icon "refresh"] in

	div ~a:[a_class "account-status alert alert-warning"] ([
		span ~a:[a_class "user"] [text username];
	] @ (optional_logout_ui (auth:>Client_auth.authenticated_user_state)) @ [
		span ~a:[a_class "offline"] [
			offline_message;
			sync_button;
		];
	])
)

let view_anonymous () =
	div ~a:[a_class "account-status login alert alert-danger"] [
		span ~a:[a_class "user"] [text "Anonymous"];
	]

let view instance =
	let view_login_form = view_login_form instance in
	let view_saved_user = view_saved_user instance in
	let view_logged_in_user = view_logged_in_user instance in
	fun {auth_state; sync_state; ui} ->
		match auth_state with
			| `Logged_out -> view_login_form None ui
			| `Failed_login username -> view_login_form (Some username) ui
			| `Anonymous -> view_anonymous ()
			| `Saved_user _ | `Saved_implicit_user _ as auth -> view_saved_user auth ui
			| `Active_user _ | `Implicit_user _ as auth -> view_logged_in_user auth sync_state

let component sync : (state, message) Ui.component =
	let command = command sync in
	Ui.component ~eq:(=) ~command ~view ()
