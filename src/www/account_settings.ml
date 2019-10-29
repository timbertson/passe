open Js_of_ocaml
open Passe
open Passe_js
open Common
module Log = (val Logging.log_module "account_settings")

open Vdoml
open Html
open Bootstrap

type user_deletion = {
	deletion_error : string option;
	password_confirm: string;
}

type password_change = {
	password_change_error: string option;
	old_password: string;
	new_password: string;
	new_confirm: string;
}

type preferences = {
	preferences_error : string option;
	password_length_input: string;
	password_length: int option;
}

type state = {
	user: Client_auth.authenticated_user_state;
	db: Store.t;
	user_deletion: user_deletion;
	password_change: password_change;
	preferences: preferences;
}

type error_dest = [
	| `user_deletion
	| `preferences
	| `password_change
]

let string_of_error_dest : error_dest -> string = function
	| `user_deletion -> "`user_deletion"
	| `preferences -> "`preferences"
	| `password_change -> "`password_change"

type text_dest = [
	| `password_length
	| `user_deletion_password
	| `password_change_old
	| `password_change_new
	| `password_change_new_confirm
]

let string_of_text_dest = function
	| `password_length -> "`password_length"
	| `user_deletion_password -> "`user_deletion_password"
	| `password_change_old -> "`password_change_old"
	| `password_change_new -> "`password_change_new"
	| `password_change_new_confirm -> "`password_change_new_confirm"

type internal_message = [
	| `db_changed of Store.t
	| `delete_account
	| `error of error_dest * string option
	| `save_preferences
	| `change_password
	| `password_changed
	| `text of text_dest * string
]

let is_sensitive : text_dest -> bool = function
	| `user_deletion_password
	| `password_change_old
	| `password_change_new
	| `password_change_new_confirm -> true

	| `password_length -> false

let string_of_message : internal_message -> string = function
	| `delete_account -> "`delete_account"
	| `error (dest, error) ->
			"`error " ^ (string_of_error_dest dest) ^
			" " ^ (Option.to_string quote_string error)
	| `save_preferences -> "`save_preferences"
	| `change_password -> "`change_password"
	| `password_changed -> "`password_changed"
	| `text (dest, text) ->
			let quote = if is_sensitive dest then mask_string else quote_string in
			"`text " ^ (string_of_text_dest dest) ^ " " ^ (quote text)
	| `db_changed _ -> "`db_changed"

let string_of_state : state -> string = fun { user; _ } ->
	"{ user = " ^ (Client_auth.string_of_auth_state (user:>Client_auth.auth_state)) ^
	" }"

type message = [
	| internal_message
	| `hide
]

let track_input dest = track_input_contents (fun str -> `text (dest, str))

let error_widget error = error
	|> Option.map (fun err ->
		p ~a:[a_class "text-danger"] [
			icon "remove";
			text err;
		]
	)
	|> Option.default empty
	|> Ui.identify (`String "error")

let password_change_section = (
	let password_input ~label ~value track =
		row `XS ~cls:"form-group" [
			col ~size:4 [control_label label];
			col [
				input ~a:[
					a_class "form-control";
					a_value value;
					a_input_type `Password;
					a_oninput track;
				] ();
			];
		]
	in

	let track_old = track_input `password_change_old in
	let track_new = track_input `password_change_new in
	let track_confirm = track_input `password_change_new_confirm in

	fun { old_password; new_password; new_confirm; password_change_error } -> (
		let any_blank = [old_password; new_password; new_confirm ]
			|> List.any (fun str -> str = "") in
		let error = if any_blank then None else password_change_error in
		let disabled = any_blank || Option.is_some error in
		div ~a:[a_class "form-horizontal"] [
			error_widget error;
			password_input ~label:"Old password" ~value:old_password track_old;
			password_input ~label:"New password" ~value:new_password track_new;
			password_input ~label:"New password (again)" ~value:new_confirm track_confirm;

			row `XS [
				col ~size:8 ~offset:4 [
					input ~a:[
						a_class "btn btn-primary submit";
						a_input_type `Submit;
						a_onclick (emitter `change_password);
						a_disabled disabled;
						a_value "Change password";
					] ();
				]
			];
		]
	)
)

let current_password_length db = Store.((get_defaults db).default_length)

let preferences_section =
	let track_password_length = track_input `password_length in

	fun { preferences; db; _ } -> (
		let { preferences_error; password_length_input; password_length; _ } = preferences in
		let password_length_unchanged = match password_length with
			| Some len when len <> (current_password_length db) -> false
			| _ -> true
		in

		let length_field = input ~a:[
			a_oninput track_password_length;
			a_value password_length_input;
			a_class "form-control";
		] () in

		div ~a:[a_class "form-horizontal"] [
			error_widget preferences_error;
			row `XS ~cls:"form-group" [
				col ~size:4 [control_label "Default password length"];
				col [length_field];
			];

			row `XS [
				col ~size:8 ~offset:4 [
					input ~a:[
						a_class "btn btn-primary";
						a_input_type `Submit;
						a_value "Save defaults";
						a_onclick (emitter `save_preferences);
						a_disabled password_length_unchanged;
					] ();
				];
			];
		]
	)

let user_deletion_section =
	let track_password_confirm = track_input `user_deletion_password in
	fun ({ deletion_error; password_confirm }:user_deletion) -> (
		let password_field =
			input ~a:[
				a_input_type `Password;
				a_class "form-control";
				a_value password_confirm;
				a_oninput track_password_confirm;
			] ()
		in

		div ~a:[a_class "form-horizontal"] [
			error_widget deletion_error;
			row `XS ~cls:"form-group" [
				col ~size:4 [control_label "Password"];
				col [password_field];
			];

			row `XS ~collapse:true [
				col ~size:8 ~offset:4 ~cls:"text-right" [
					span ~a:[a_class "text-muted"] [text "Careful now... "];
					input ~a:[
						a_class "btn btn-danger submit";
						a_input_type `Submit;
						a_value "Delete account";
						a_onclick (emitter `delete_account);
					] ();
				];
			];
		]
	)

let view_panel instance =
	let overlay = Bootstrap.overlay instance ~cancel:`hide in
	let panel ~title children = overlay [Bootstrap.panel ~close:`hide ~title children] in
	(fun state ->
		panel ~title:"Account settings" ([
			preferences_section state;
			hr ()
		] @ (match state.user with
				| `Active_user _ | `Saved_user _ -> [
					password_change_section state.password_change;
					hr ();
					user_deletion_section state.user_deletion;
				]
				| `Implicit_user _ | `Saved_implicit_user _ -> []
			)
		)
	)

let button : (unit, unit) Ui.component = Ui.component
	~eq:(Pervasives.(=))
	~view:(fun _instance _state -> a ~a:[
		a_class "hover-reveal link settings-button";
		a_title "Settings";
		a_onclick (emitter ());
	] [icon "cog"])
	()

let delete_account ~set_auth_state instance password token = (
	if (Dom_html.window##confirm (Js.string "Are you SURE?") |> Js.to_bool) then (
		Ui.emit instance (`error (`user_deletion, None));
		let open Server in
		let open Lwt in
		let return_error msg = Ui.emit instance (`error (`user_deletion, Some msg)); return_unit in
		Some (
			match%lwt call Client_auth.delete_user_api
				~token (`Assoc ["password", `String password])
			with
				| OK () ->
					set_auth_state `Logged_out;
					Ui.emit instance `hide;
					return_unit
				| Unauthorized msg ->
					return_error (msg |> Option.default "Unauthorized")
				| Failed (_, msg,_) ->
					return_error msg
		)
	) else None
)

let save_preferences ~sync _instance { preferences; db; _ } = (
	let { password_length; _ } = preferences in
	let current = current_password_length db in
	let password_length = password_length |> Option.default current in
	let saved = if current = password_length
		then true
		else Sync.save_default ~state:sync (`Length password_length)
	in
	if saved then (
		Dom_html.window##alert (Js.string "Defaults saved")
	) else (
		Dom_html.window##alert (Js.string "Unable to save DB")
	)
)

let change_password ~sync instance password_change token = (
	let { old_password; new_password; new_confirm; _ } = password_change in
	let data = `Assoc [
		"old", `String old_password;
		"new", `String new_password;
		"new2", `String new_confirm;
	] in
	let set_error value = Ui.emit instance (`error (`password_change, value)) in

	let set_auth_state = sync.Sync.set_auth_state in
	let open Server in
	let open Lwt in
	match%lwt call Client_auth.change_password_api ~token data with
		| OK creds ->
			Dom_html.window##alert (Js.string "Password changed.");
			set_auth_state (`Active_user creds);
			Ui.emit instance `password_changed;
			return_unit
		| Unauthorized msg ->
			set_error (Some (msg |> Option.default "Unauthorized"));
			return_unit;
		| Failed (_, msg,_) ->
			set_error (Some msg);
			return_unit
)

let command sync instance =
	let set_auth_state = sync.Sync.set_auth_state in
	let delete_account = delete_account ~set_auth_state in
	(fun state msg -> match msg with
		| `delete_account ->
			Client_auth.token_of_authenticated state.user
				|> Option.bind (delete_account instance state.user_deletion.password_confirm)
		| `change_password ->
			Client_auth.token_of_authenticated state.user
				|> Option.map (change_password ~sync instance state.password_change)
		| `save_preferences -> save_preferences ~sync instance state; None
		| _ -> None
	)

let panel sync : (state, message) Ui.component =
	Ui.component ~view:view_panel ~eq:(=) ~command:(command sync) ()

let reset_preferences state =
	let current_length = current_password_length state.db in
	{ state with
		preferences = {
			password_length_input = current_length |> string_of_int;
			password_length = None;
			preferences_error = None;
		}
	}

type external_state = Store.t
let external_state sync : external_state React.signal = sync.Sync.db_fallback
let external_messages db = [ `db_changed db ]

let initial_password_change = {
	password_change_error = None;
	old_password = "";
	new_password = "";
	new_confirm = "";
}

let initial db auth_state =
	{
		user = auth_state;
		user_deletion = {
			deletion_error = None;
			password_confirm = "";
		};
		preferences = {
			preferences_error = None;
			password_length_input = "";
			password_length = None;
		};
		password_change = initial_password_change;
		db;
	} |> reset_preferences

let update state : internal_message -> state = function
	| `error (dest, err) ->
		(match dest with
			| `user_deletion ->
				{ state with
					user_deletion = { state.user_deletion with
						deletion_error = err
					}
				}
			| `preferences ->
				{ state with
					preferences = { state.preferences with
						preferences_error = err
					}
				}
			| `password_change ->
				{ state with
					password_change = { state.password_change with
						password_change_error = err
					}
				}
		)
	| `text (dest, contents) ->
		let update_password_change_error password_change =
			let error = if password_change.new_password <> password_change.new_confirm
				then Some "Passwords don't match"
				else None
			in
			{ password_change with password_change_error = error }
		in
		(match dest with
			| `password_length ->
				let prefs length err = {
					password_length_input = contents;
					password_length = length;
					preferences_error = err;
				} in
				let preferences = match (try Some (int_of_string contents) with _ -> None) with
					| None -> prefs None (Some "Not a number")
					| Some len -> prefs (Some len) None
				in { state with preferences }
			| `user_deletion_password ->
				{ state with user_deletion = { state.user_deletion with
					password_confirm = contents }
				}
			| `password_change_old ->
				{ state with password_change = { state.password_change with
					old_password = contents } |> update_password_change_error
				}
			| `password_change_new ->
				{ state with password_change = { state.password_change with
					new_password = contents } |> update_password_change_error
				}
			| `password_change_new_confirm ->
				{ state with password_change = { state.password_change with
					new_confirm = contents } |> update_password_change_error
				}
		)
	
	| `password_changed ->
			{ state with password_change = initial_password_change }

	(* command-only messages *)
	| `delete_account -> state
	| `save_preferences -> state
	| `change_password -> state
	| `db_changed db ->
		{ state with db } |> reset_preferences
