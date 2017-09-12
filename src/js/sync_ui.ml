open Passe
open Passe_js
open React_ext
open Lwt
open Common
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
	sync_time_desc: string option;
}

type state = {
	auth_state: Client_auth.auth_state;
	sync_state: Sync.sync_state;
	ui: ui_state;
}

type external_state = Sync.sync_state * Client_auth.auth_state

let string_of_login_form = function { username; password } ->
	"{ username = " ^ (Option.to_string quote_string username) ^
	"; password = " ^ (Option.to_string mask_string password) ^
	" }"

let string_of_ui_state = function { error; busy; login_form; sync_time_desc } ->
	"{ error = " ^ (Option.to_string quote_string error) ^
	"; busy = " ^ (string_of_bool busy) ^
	"; login_form = " ^ (string_of_login_form login_form) ^
	"; sync_time_desc = " ^ (Option.to_string quote_string sync_time_desc) ^
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
	| `sync_time_desc of string option
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
	| `sync_time_desc s -> "`sync_time_desc " ^ (Option.to_string quote_string s)
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
	match%lwt call Client_auth.logout_api token with
		| OK () | Unauthorized _ ->
			set_auth_state `Logged_out;
			return_unit;
		| Failed (_, msg,_) ->
			Log.err (fun m->m "Can't log out: %s" msg);
			return_unit
)

let submit_form ~(set_auth_state:Client_auth.auth_state -> unit) ~emit api auth_state {username; password} = (
	Log.info (fun m->m "form submitted");
	let username = username |> Option.or_ (Client_auth.uid_of_state auth_state) in
	let data = match username, password with
		| Some username, Some password ->
			Some (`Assoc [
				"user", `String username;
				"password", `String password;
			])
		| None, _ -> emit (`error (Some "Username required")); None
		| _, None -> emit (`error (Some "Password required")); None
	in
	data |> Option.map (fun data ->
		emit (`error None);
		Server.call api data |> Lwt.map (function
			| Server.OK creds ->
				set_auth_state (`Active_user creds)
			| Server.Failed (_, message, _) ->
				emit (`error (Some message))
			| Server.Unauthorized _ -> assert false
		)
	) |> Option.default (Lwt.return_unit)
)

let auth_loop ~(set_auth_state:Client_auth.auth_state -> unit) ~emit (auth:Client_auth.saved_auth_state) = (
	let continue = ref true in
	while%lwt !continue do
		emit (`busy true);
		(try%lwt
			(
				continue := false;
				let open Server in
				let response = match auth with
					| `Saved_user ((username, token) as creds) ->
						Server.call Client_auth.token_validate_api token |> Lwt.map (Response.map (fun valid ->
							if valid then `Active_user creds else `Failed_login username
						))
					| `Saved_implicit_user _ ->
						Server.call Client_auth.server_state_api J.empty |> Lwt.map (Response.map (function
							| Some u -> `Implicit_user u | None -> `Anonymous
						))
				in

				match%lwt response with
				| OK user -> set_auth_state user; return_unit
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
				)
			with e -> raise e
		) [%lwt.finally
			emit (`busy false);
			Lwt.return_unit
		]
	done
)

let sync_db_loop ~sync ~sync_state ~emit auth =
	(* used when signed in, to periodically sync DB state *)
	let continue = ref true in
	while%lwt !continue do
		Log.info (fun m->m "sync loop running..");
		(match%lwt sync.run_sync (auth:>Client_auth.authenticated_user_state) with
			| Error msg ->
				Log.err (fun m->m"sync loop failed: %s" msg);
				emit (`error (Some msg));
				continue := false;
				return_unit
			| Ok () ->
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
		)
	done

let update_sync_time ~sync_state ~emit () =
	while%lwt true do
		let next_change = sync_state |> events_of_signal |> Lwt_react.E.next in
		let () = match S.value sync_state with
			| Uptodate | Syncing | Local_changes _ ->
				emit (`sync_time_desc None)
			| Updated_at date ->
				let now = Date.time () in
				let time_diff = max 0.0 (now -. date) in
				let desc = (Date.human_time_span_desc time_diff) ^ " ago" in
				emit (`sync_time_desc (Some desc))
		in
		Lwt.pick [
			next_change |> Lwt.map ignore;
			Lwt_js.sleep 60.0;
		]
	done

let run_background_sync ~sync ~set_auth_state ~emit sync_state : (Client_auth.auth_state option -> unit Lwt.t) =
	let last_auth = ref None in
	let run = (fun auth ->
		let open Client_auth in
		Log.info (fun m->m "run_background_sync called with auth: %s" (string_of_auth_state auth));
		let previous_auth = !last_auth in
		last_auth := Some auth;
		(match auth with
			| `Logged_out | `Anonymous | `Failed_login _ -> return_unit
			| `Saved_user _ | `Saved_implicit_user _ as u ->
				let prev_uid = Option.bind uid_of_state previous_auth
				and current_uid = uid_of_authenticated (u:>authenticated_user_state) in
				let%lwt () = if prev_uid = Some(current_uid)
					(* slow down reconnect attempts if user is unchanged *)
					then (
						Log.debug (fun m->m "delaying sync attempt for unchanged user");
						Lwt_js.sleep 60.0
					) else return_unit
				in
				auth_loop ~set_auth_state ~emit (u:>saved_auth_state)
			| `Active_user _ | `Implicit_user _ as u ->
				Lwt.pick [
					sync_db_loop ~sync ~sync_state ~emit (u:>logged_in_user_state);
					update_sync_time ~sync_state ~emit ();
				]
		) >>= fun _ -> return_unit
	) in
	fun auth -> auth
		|> Option.or_ !last_auth
		|> Option.map run
		|> Option.default Lwt.return_unit

(* note: not attached to this instance, since it needs to respond to
 * events that come from above this component *)
(* TODO: should vdoml have some standard way of distributing
 * commands for this sort of case? *)
let command ~sync ~do_async ~emit : (state, internal_message) Ui.command_fn  = (
	let set_auth_state = sync.Sync.set_auth_state in
	let logout = Ui.supplantable (fun token ->
		logout ~set_auth_state token
	) in

	let login = Ui.supplantable (fun (state:state) ->
		submit_form ~set_auth_state ~emit Client_auth.login_api
			state.auth_state state.ui.login_form
	) in

	let signup = Ui.supplantable (fun (state:state) ->
		submit_form ~set_auth_state ~emit Client_auth.signup_api
			state.auth_state state.ui.login_form
	) in

	let sync_state = sync_state sync in
	let background_sync = Ui.supplantable
		(run_background_sync ~sync ~set_auth_state ~emit sync_state) in

	let latest_auth = ref None in
	let background_sync auth =
		latest_auth := auth;
		background_sync auth
	in

	(* kick off initial background syc *)
	background_sync (Some (S.value sync.Sync.auth_state)) |> do_async;

	fun state message ->
		match message with
		| `request_logout token -> Some (logout token)
		| `request_login -> Some (login state)
		| `request_signup -> Some (signup state)
		| `auth_state auth ->
			if !latest_auth <> (Some auth) then (
				Log.debug (fun m->m"resetting background_sync with new auth state %s" (Client_auth.string_of_auth_state auth));
				Some (background_sync (Some auth))
			) else (
				Log.debug (fun m->m"ignoring duplicate auth state %s" (Client_auth.string_of_auth_state auth));
				None
			)
		| `request_sync -> Some (background_sync None)
		| _ -> None
)

let update state (message:internal_message) =
	let update_ui state message = match message with
		| `error error -> { state with error }
		| `busy busy -> { state with busy }
		| `sync_time_desc sync_time_desc -> { state with sync_time_desc }
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
	| `error _ | `busy _ | `edit _ | `sync_time_desc _ as message -> { state with ui = update_ui state.ui message }

	(* used only for `command` side effects *)
	| `request_sync | `request_logout _ | `request_login | `request_signup
		-> state

let initial ((sync_state, auth_state):external_state) = {
	auth_state;
	sync_state;
	ui = {
		sync_time_desc = None;
		error = None;
		busy = false;
		login_form = {
			username = None;
			password = None;
		};
	};
}

type sync_state = {
	sync_error : string option;
	sync_auth : Client_auth.auth_state;
}

open Html
open Bootstrap

let view_sync_state ~sync_time_desc = (function
	| Uptodate -> text "";
	| Updated_at _ ->
			let attrs = [
				a_onclick (emitter `request_sync);
				a_class "link has-tooltip";
			] @ (match sync_time_desc with
				| Some desc -> [a_title ("Last sync " ^ desc)]
				| None -> []
			) in
			a ~a:attrs [icon "refresh"]
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
		|> Option.filter (fun evt -> (Keycode_ext.of_event evt) = Keycode_ext.Enter)
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
	fun (auth:Client_auth.authenticated_user_state) { sync_time_desc; _} sync_state ->
		let username = Client_auth.name_of_authenticated auth in
		div ~a:[a_class "account-status alert alert-success"] ([
			span ~a:[a_class "user"] [text username];
		] @ (optional_logout_ui auth) @ [
			span ~a:[a_class "online"] [
				account_settings_button ();
				view_sync_state ~sync_time_desc sync_state;
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
			| `Active_user _ | `Implicit_user _ as auth -> view_logged_in_user auth ui sync_state

let pair a b = a,b
let external_state sync : external_state React.signal =
	let open Sync in S.l2 (fun a b -> a,b)
	(sync_state sync)
	sync.auth_state

let external_messages (sync, auth) = [ `sync_state sync; `auth_state auth ]

let component : (state, message) Ui.component =
	Ui.component ~eq:(=) ~view ()
