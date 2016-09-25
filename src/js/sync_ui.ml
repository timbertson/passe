open Passe
open Passe_js
open Common
open React_ext
open Passe_ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest
open Sync

type error_message = string
type ui_state = {
	error: error_message option;
	busy: bool;
}

type state = {
	auth_state: Client_auth.auth_state;
	sync_state: Sync.sync_state;
	ui: ui_state;
}

let string_of_ui_state = function { error; busy } ->
	"{ error = " ^ (Option.to_string quote_string error) ^
	"; busy = " ^ (string_of_bool busy) ^
	" }"

let string_of_state = function { auth_state; sync_state; ui } ->
	"{ auth_state = " ^ (Client_auth.string_of_auth_state auth_state) ^
	"; sync_state = " ^ (Sync.string_of_sync_state sync_state) ^
	"; ui = " ^ (string_of_ui_state ui) ^
	" }"

type ui_message = [
	| `error of error_message option
	| `busy of bool
	| `request_sync
]

type message = [
	| ui_message
	| `auth_state of Client_auth.auth_state
	| `sync_state of Sync.sync_state
]

let string_of_message : message -> string = function
	| `auth_state s -> "`auth_state " ^ (Client_auth.string_of_auth_state s)
	| `sync_state s -> "`sync_state " ^ (Sync.string_of_sync_state s)
	| `error err -> "`error " ^ (Option.to_string quote_string err)
	| `busy b -> "`busy " ^ (string_of_bool b)
	| `request_sync -> "`request_sync"

let run_background_auth ~set_auth_state ~sync_requested () =
	(* used for saved user, to automatically login *)
	let auth_messages, emit = E.create () in
	let run = (fun (auth:Client_auth.saved_auth_state) ->
		let continue = ref true in
		while_lwt !continue do
			emit (`busy true);
			try_lwt (
				continue := false;
				let open Server in
				let response = match auth with
					| `Saved_user (username, creds) ->
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
					Lwt.pick [
						(Lwt_condition.wait sync_requested);
						(Lwt_js.sleep 60.0);
					]
			) finally (
				emit (`busy false);
				Lwt.return_unit
			)
		done
	) in
	(auth_messages, run)

let events_of_signal s =
	let e, emit = E.create () in
	emit (S.value s);
	E.select [e; S.changes s]

let run_background_sync ~sync_requested sync sync_state =
	(* used when signed in, to periodically sync DB state *)
	let run_sync auth =
		(* XXX todo: uncomment this when we track down the error *)
		try_lwt
			sync.run_sync auth
		with e -> begin
			Log.err (fun m->m "%s" (Printexc.to_string e));
			raise e
		end
	in

	fun (auth:Client_auth.logged_in_user_state) -> (
		while_lwt true do
			Log.info (fun m->m "sync loop running..");
			lwt () = run_sync (auth:>Client_auth.authenticated_user_state) in
			let next_change = (sync_state
				|> events_of_signal
				|> E.filter (function | Local_changes _ -> true | _ -> false)
				|> Lwt_react.E.next)
			in
			Lwt.pick [
				(* when clicked *)
				(Lwt_condition.wait sync_requested);
				(* every 30 minutes *)
				(Lwt_js.sleep 18000.0);
				(* shortly after making a change to the DB *)
				(next_change >>= (fun _ -> Lwt_js.sleep 2.0))
			]
		done
	)

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

let update sync =
	let sync_state = sync_state sync in

	let background_thread = ref None in
	let cancel_background () =
		!background_thread |> Option.may (fun th ->
			Log.debug(fun m->m"cancelling background task");
			Lwt.cancel th;
			background_thread := None
		) in

	let replace_thread desc th =
		cancel_background ();
		Log.debug(fun m->m"setting new background task: %s" desc);
		background_thread := Some th
	in

	let sync_requested = Lwt_condition.create () in

	(* set up background functions *)
	let run_background_sync = run_background_sync ~sync_requested sync sync_state in
	let auth_messages, run_background_auth =
		run_background_auth ~set_auth_state:sync.Sync.set_auth_state ~sync_requested () in

	let observed_auth_state = sync.Sync.auth_state |> S.map (fun auth ->
		cancel_background ();
		let () = match auth with
			| `Saved_user _ | `Saved_implicit_user _ as auth ->
				replace_thread "auth" (run_background_auth auth)
			| `Implicit_user _ | `Active_user _ as auth ->
				replace_thread "sync" (run_background_sync auth)
			| `Logged_out | `Failed_login _ | `Anonymous -> ()
		in
		auth
	) in

	let auth_state_messages = observed_auth_state |> S.changes |> E.map (fun s -> `auth_state s) in
	let sync_state_messages = sync_state |> S.changes |> E.map (fun s -> `sync_state s) in
	let update_ui state message = match message with
		| `error error -> { state with error }
		| `busy busy -> { state with busy }
	in
	let update state (message:message) =
		match message with
		| `auth_state auth_state -> { state with auth_state }
		| `sync_state sync_state -> { state with sync_state }
		| `error _ | `busy _ as message -> { state with ui = update_ui state.ui message }
		| `request_sync ->
			Log.debug (fun m->m "sync requested");
			Lwt_condition.broadcast sync_requested ();
			state
	in
	let messages : message event = E.select [
		auth_messages;
		auth_state_messages;
		sync_state_messages
	] in
	let initial = {
		ui = { error = None; busy = false };
		auth_state = sync.Sync.auth_state |> S.value;
		sync_state = sync_state |> S.value;
	} in
	(initial, update, messages)

type sync_state = {
	sync_error : string option;
	sync_auth : Client_auth.auth_state;
}

let ui state = (
	let last_sync_signal = state.last_sync#signal in
	let last_sync_time = last_sync_signal |> signal_lift_opt (function
		| `Float t -> t
		| _ -> raise @@ AssertionError ("invalid `last_sync` value")
	) in

	let set_auth_state = state.set_auth_state in

	let sync_running = state.sync_running in
	let run_sync auth =
		try_lwt
			state.run_sync auth
		with e -> begin
			Log.err (fun m->m "%s" (Printexc.to_string e));
			return ()
		end
	in

	let sync_state = S.bind sync_running (fun is_running ->
		if is_running then S.const Syncing else (
			S.l2 (fun db sync_time ->
				let len = List.length db.Store.changes in
				if len = 0 then
					sync_time |> Option.map (fun t -> Updated_at t) |> Option.default Uptodate
				else Local_changes len
			) state.db_fallback last_sync_time
		)
	) in

	let login_form (username:string option) = (
		let error, set_error = S.create None in

		let error_widget = error
			|> optional_signal_content (fun err -> Passe_ui.p ~cls:"text-danger" ~text:err ~children:[
				child i ~cls:"glyphicon glyphicon-remove" ();
			] ())
			|> Passe_ui.stream in
		
		div ~cls:"account-status login alert alert-info" ~children:[
			child div ~cls:"login form-inline" ~attrs:["role","form"] ~children:[
				error_widget;
				child div ~cls:"form-group form-group-xs email" ~children:[
					child label ~cls:"sr-only" ~text:"User" ();
					child input ~cls:"form-control username-input" ~attrs:[
						("name","user");
						("placeholder","User");
						("type","text");
						("value", Option.default "" username);
					] ();
				] ();
				space;

				child div ~cls:"form-group form-group-xs password" ~children:[
					child label ~cls:"sr-only" ~text:"password" ();
					child input ~cls:"form-control password-input" ~attrs:[
						("type","password");
						("name","password");
						("placeholder","password")
					] ();
				] ();

				space;
				child input ~cls:"btn btn-primary login" ~attrs:[("type","submit"); ("value","Sign in")] ();
				child input ~cls:"btn btn-default muted signup pull-right" ~attrs:[("type","button"); ("value","Register")] ~text:"Register" ();

				child div ~cls:"clearfix" ();
			] ~mechanism:(fun elem ->
				let signup_button = elem##querySelector(Js.string ".signup") |> non_null in
				let login_button = elem##querySelector(Js.string ".login") |> non_null in
				(* XXX just search for any `input`, rather than binding events on each of these individually *)
				let password_input = elem##querySelector(Js.string ".password-input") |> non_null in
				let username_input = elem##querySelector(Js.string ".username-input") |> non_null in
				let submit url =
					Log.info (fun m->m "form submitted");
					let data = `Assoc (get_form_contents elem
						|> List.map (fun (name, value) -> (name, `String value))) in
					let open Server in
					let open Either in
					set_error None;
					lwt response = Server.post_json ~data url in
					let open Server in
					let () = match response with
						| OK response ->
							set_auth_state (`Active_user (Auth.get_response_credentials response))
						| Failed (_, message, _) ->
								set_error (Some message)
						| Unauthorized _ -> assert false
					in
					return_unit
				in
				let submit_on_return event _ =
					if event##keyCode = Keycode.return then (
						stop event;
						submit Client_auth.login_url
					) else return_unit
				in

				Lwt_js_events.clicks signup_button (fun event _ ->
					stop event;
					submit Client_auth.signup_url
				)
				<&>
				Lwt_js_events.clicks login_button (fun event _ ->
					stop event;
					submit Client_auth.login_url
				)
				<&> Lwt_js_events.keydowns password_input submit_on_return
				<&> Lwt_js_events.keydowns username_input submit_on_return
			) ()
		] ()
	) in

	let anonymous_ui () =
		div ~cls:"account-status login alert alert-danger" ~children:[
			child span ~cls:"user" ~text:"Anonymous" ();
		] ()
	in

	let sync_state_widget auth = (
		let open Passe_ui in
		let _node w = (w:>Dom.node widget_t) in
		let sync_mechanism = (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				lwt () = run_sync auth in
				return (App_cache.update ())
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
						~mechanism:sync_mechanism ()
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
						~mechanism:sync_mechanism ()
				] ()
		) |> stream
	) in

	let account_settings_button user = (

		let preferences_section ~close () = (
			let error, set_error = S.create None in
			let error_widget = error
				|> optional_signal_content (fun err -> Passe_ui.p ~cls:"text-danger" ~text:err ~children:[
					icon "remove";
				] ())
				|> Passe_ui.stream in
			let db = S.value state.db_fallback in
			let current_length = Store.((get_defaults db).default_length) in
			let length, set_length = S.create current_length in
			let set_length str =
				match (try Some (int_of_string str) with _ -> None) with
					| Some len -> set_length len
					| None -> set_error (Some "Not a number")
			in
			let length_field = input_of_signal ~update:set_length (S.map string_of_int length) in
			length_field#attr "class" "form-control";

			child div ~cls:"form-horizontal" ~children:[
				error_widget;

				row `XS ~cls:"form-group" [
					col ~size:4 [control_label "Default password length"];
					col [frag length_field];
				];

				row `XS [
					col ~size:8 ~offset:4 [
						child input ~cls:"btn btn-primary save" ~attrs:[("type","submit"); ("value","Save defaults")] ();
					];
				];
			] ~mechanism:(fun form ->
				let submit_button = form##querySelector(Js.string ".save") |> non_null in
				Lwt_js_events.clicks submit_button (fun event _ ->
					stop event;
					let length = S.value length in
					let saved = if length = current_length
						then true
						else Sync.save_default ~state (`Length length)
					in
					if saved then (
						Dom_html.window##alert (Js.string "Defaults saved");
						close ();
					) else (
						Dom_html.window##alert (Js.string "Unable to save DB");
					);
					return_unit
				)
			) ();
		) in

		let password_change_section ~token ~close () = (
			let error, set_error = S.create None in
			let error_widget = error
				|> optional_signal_content (fun err -> Passe_ui.p ~cls:"text-danger" ~text:err ~children:[
					child i ~cls:"glyphicon glyphicon-remove" ();
				] ())
				|> Passe_ui.stream in
			let password_input ~label name =
				let attrs = ["name",name; "type","password"] in
				row `XS ~cls:"form-group" [
					col ~size:4 [control_label label];
					col [child input ~cls:"form-control" ~attrs:attrs ()];
				]
			in

			child div ~cls:"form-horizontal" ~children:[
				error_widget;
				password_input ~label:"Old password" "new";
				password_input ~label:"New password" "new";
				password_input ~label:"New password (again)" "new2";

				row `XS [
					col ~size:8 ~offset:4 [
						child input ~cls:"btn btn-primary submit" ~attrs:[("type","submit"); ("value","Change password")] ();
					]
				];
			] ~mechanism:(fun form ->
				let username = Client_auth.name_of_authenticated user in
				let submit_button = form##querySelector(Js.string ".submit") |> non_null in
				Lwt_js_events.clicks submit_button (fun event _ ->
					stop event;
					let pairs = get_form_contents form in
					let data = `Assoc (pairs |> List.map (fun (a, b) -> a, `String b)) in
					let new1 = data |> J.mandatory J.string_field "new"
					and new2 = data |> J.mandatory J.string_field "new2" in
					if new1 <> new2 then (
						set_error (Some "Passwords don't match");
						return_unit
					) else (
						let open Server in
						match_lwt post_json ~token ~data Client_auth.change_password_url with
							| OK creds ->
								Dom_html.window##alert (Js.string "Password changed.");
								set_auth_state (`Active_user (username, creds));
								close ();
								return_unit
							| Unauthorized msg ->
								set_error (Some (msg |> Option.default "Unauthorized"));
								return_unit;
							| Failed (_, msg,_) ->
								set_error (Some msg);
								return_unit
					)
				)
			) ()
		) in

		let user_delete_section ~token ~close () = (
			let error, set_error = S.create None in
			let error_widget = error
				|> optional_signal_content (fun err -> Passe_ui.p ~cls:"text-danger" ~text:err ~children:[
					child i ~cls:"glyphicon glyphicon-remove" ();
				] ())
				|> Passe_ui.stream in
			let password, set_password = S.create "" in
			let password_field = input_of_signal ~update:set_password password in
			let () = (
				password_field#attr "type" "password";
				password_field#attr "class" "form-control";
			) in

			child div ~cls:"form-horizontal" ~children:[
				error_widget;
				row `XS ~cls:"form-group" [
					col ~size:4 [control_label "Password"];
					col [frag password_field ];
				];

				row `XS ~collapse:true [
					col ~size:8 ~offset:4 ~cls:"text-right" [
						child span ~cls:"text-muted" ~text:"Careful now... " ();
						child input ~cls:"btn btn-danger submit" ~attrs:[("type","submit"); ("value","Delete account")] ();
					];
				];
			] ~mechanism:(fun form ->
				let submit_button = form##querySelector(Js.string ".submit") |> non_null in
				Lwt_js_events.clicks submit_button (fun event _ ->
					stop event;
					if (Dom_html.window##confirm (Js.string "Are you SURE?") |> Js.to_bool) then begin
						set_error None;
						let open Server in
						match_lwt post_json
							~token
							~data:(`Assoc ["password", `String (S.value password)])
							Client_auth.delete_user_url
						with
							| OK _ ->
								set_auth_state `Logged_out;
								close ();
								return_unit;
							| Unauthorized msg ->
								set_error (Some (msg |> Option.default "Unauthorized"));
								return_unit;
							| Failed (_, msg,_) ->
								set_error (Some msg);
								return_unit
					end else return_unit
				)
			) ();
		) in

		child a
		~cls:"hover-reveal link settings-button"
		~attrs:["title", "Settings"]
		~children:[icon "cog"]
		~mechanism:(fun elem ->
			Lwt_js_events.clicks elem (fun evt _ ->
				Passe_ui.overlay (fun close ->
					Passe_ui.panel ~close ~title:"Account settings" ~children:[
						child div ~children:([
								preferences_section ~close ();
								child hr ()
							] @ (match user with
								| `Active_user (_, token)  | `Saved_user (_, token) -> [
									password_change_section ~close ~token ();
									child hr ();
									user_delete_section ~close ~token ();
								]
								| `Implicit_user _ | `Saved_implicit_user _ -> []
							)
						) ()
					] ()
				)
			)
		) ()
	) in


	let sync_debounce = ref return_unit in

	let auth_ui (auth: Client_auth.auth_state) =
		Log.info (fun m->m "Auth state: %s" (Client_auth.string_of_auth_state auth));

		let sync_mechanism auth = fun elem ->
			let run_sync () =
				lwt () = !sync_debounce in
				sync_debounce := (Lwt_js.sleep 5.0);
				run_sync auth
				in
			lwt () = run_sync () in
			while_lwt true do
				Log.info (fun m->m "sync loop running..");
				Lwt.pick [
					(* every 30 minutes, attempt a sync *)
					(Lwt_js.sleep 18000.0 >>= run_sync);
					(* also sync shortly after making a change to the DB *)
					(abortable_stream_mechanism sync_state (function
						| Local_changes _ -> Lwt_js.sleep 2.0 >>= run_sync
						| _ -> return_unit
					));
				]
			done
		in

		let optional_logout_ui (auth:Client_auth.authenticated_user_state) =
			let logout_button tooltip token = (child a
				~cls:"hover-reveal link"
				~attrs:["title",tooltip]
				~children:[icon "remove"]
				~mechanism:(fun elem ->
					Lwt_js_events.clicks elem (fun evt _ ->
						let open Server in
						match_lwt post_json ~data:token Client_auth.logout_url with
							| OK _ | Unauthorized _ ->
								set_auth_state `Logged_out;
								return_unit;
							| Failed (_, msg,_) ->
								Log.err (fun m->m "Can't log out: %s" msg);
								return_unit
					)
				) ()
			) in
			match Client_auth.token_of_authenticated auth with
			| Some token -> [ logout_button "Log out" token ]
			| None -> []
		in

		let logged_in_ui (auth:Client_auth.authenticated_user_state) =
			let username = Client_auth.name_of_authenticated auth in
			div ~cls:"account-status alert alert-success" ~children:([
				child span ~cls:"user" ~text:username ();
			] @ (optional_logout_ui auth) @ [
				child span ~cls:"online" ~children:[
					account_settings_button auth;
					sync_state_widget auth;
				] ()
			]) ~mechanism:(sync_mechanism auth) ()
		in

		let saved_user_ui (auth: Auth.saved_auth_state) = (
			let username = Auth.name_of_saved auth in
			let busy, set_busy = S.create true in
			let mechanism = (fun elem ->
				let continue = ref true in
				while_lwt !continue do
					set_busy true;
					continue := false;
					let open Server in
					let response = match auth with
						| `Saved_user (username, creds) ->
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
						set_busy false;
						Lwt.pick [
							(
								lwt (_:Dom_html.mouseEvent Js.t) = Lwt_js_events.click elem in
								return_unit
							);
							(Lwt_js.sleep 60.0);
						]
				done
			) in

			let offline_message = span ~text:"offline " () in
			offline_message#class_s "hidden" busy;

			let sync_button = a ~children:[icon "refresh"] () in
			sync_button#class_s "syncing" busy;
			sync_button#class_s "link" (S.map not busy);

			div ~cls:"account-status alert alert-warning" ~children:([
				child span ~cls:"user" ~text:username ();
			] @ (optional_logout_ui (auth:>Client_auth.authenticated_user_state)) @ [
				child span ~cls:"offline" ~children:[
					frag offline_message;
					frag sync_button;
				] ~mechanism ()
			]) ()
		) in

		match auth with
			| `Logged_out -> login_form None
			| `Failed_login username -> login_form (Some username)
			| `Anonymous -> anonymous_ui ()
			| `Saved_user _ as auth -> saved_user_ui auth
			| `Saved_implicit_user _ as auth -> saved_user_ui auth
			| `Implicit_user _ as auth -> logged_in_ui auth
			| `Active_user _ as auth -> logged_in_ui auth
	in

	state.auth_state |> S.map auth_ui |> Passe_ui.stream
)

open Vdoml
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
				a_title ("Last sync " ^ (Date.human_time_span_desc time_diff) ^ " ago");
			] [icon "refresh"]
	| Syncing ->
		span ~a:[a_class "syncing"] [icon "refresh"]
	| Local_changes count ->
		span ~a:[a_title ((string_of_int count) ^ " pending changes...")] [
			a ~a:[a_class "link"; a_onclick (emitter `request_sync)] [icon "upload"]
		]
	)

let view_login_form instance = fun username {error; _} -> (
	let space = text " " in
	let error_text = error |> Option.map (fun err ->
		p ~a:[a_class "text-danger"] [
			text err;
			Bootstrap.icon "remove";
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
					a_value (Option.default "" username);
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
				] ();
			];

			space;
			input ~a:[
				a_class "btn btn-primary login";
				a_input_type `Submit;
				a_value "Sign in";
			] ();
			input ~a:[
				a_class "btn btn-default muted signup pull-right";
				a_input_type `Button;
				a_value "Register";
			] ();

			div ~a:[a_class "clearfix"] [];
		])
		(* ~mechanism:(fun elem -> *)
		(* 	let signup_button = elem##querySelector(Js.string ".signup") |> non_null in *)
		(* 	let login_button = elem##querySelector(Js.string ".login") |> non_null in *)
		(* 	(* XXX just search for any `input`, rather than binding events on each of these individually *) *)
		(* 	let password_input = elem##querySelector(Js.string ".password-input") |> non_null in *)
		(* 	let username_input = elem##querySelector(Js.string ".username-input") |> non_null in *)
		(* 	let submit url = *)
		(* 		Log.info (fun m->m "form submitted"); *)
		(* 		let data = `Assoc (get_form_contents elem *)
		(* 			|> List.map (fun (name, value) -> (name, `String value))) in *)
		(* 		let open Server in *)
		(* 		let open Either in *)
		(* 		set_error None; *)
		(* 		lwt response = Server.post_json ~data url in *)
		(* 		let open Server in *)
		(* 		let () = match response with *)
		(* 			| OK response -> *)
		(* 				set_auth_state (`Active_user (Auth.get_response_credentials response)) *)
		(* 			| Failed (_, message, _) -> *)
		(* 					set_error (Some message) *)
		(* 			| Unauthorized _ -> assert false *)
		(* 		in *)
		(* 		return_unit *)
		(* 	in *)
		(* 	let submit_on_return event _ = *)
		(* 		if event##keyCode = Keycode.return then ( *)
		(* 			stop event; *)
		(* 			submit Client_auth.login_url *)
		(* 		) else return_unit *)
		(* 	in *)
		(*  *)
		(* 	Lwt_js_events.clicks signup_button (fun event _ -> *)
		(* 		stop event; *)
		(* 		submit Client_auth.signup_url *)
		(* 	) *)
		(* 	<&> *)
		(* 	Lwt_js_events.clicks login_button (fun event _ -> *)
		(* 		stop event; *)
		(* 		submit Client_auth.login_url *)
		(* 	) *)
		(* 	<&> Lwt_js_events.keydowns password_input submit_on_return *)
		(* 	<&> Lwt_js_events.keydowns username_input submit_on_return *)
		(* ) () *)
	];
)

let account_settings_button user = text "TODO"

let optional_logout_ui user = [text "TODO"]

let view_logged_in_user _instance = fun (auth:Client_auth.authenticated_user_state) sync_state ->
	let username = Client_auth.name_of_authenticated auth in
	div ~a:[a_class "account-status alert alert-success"] ([
		span ~a:[a_class "user"] [text username];
	] @ (optional_logout_ui auth) @ [
		span ~a:[a_class "online"] [
			account_settings_button auth;
			view_sync_state sync_state;
		];
	])

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
