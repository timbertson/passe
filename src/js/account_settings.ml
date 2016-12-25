open Passe
open Passe_js
open Common
module Log = (val Logging.log_module "account_settings")

open Vdoml
open Html
open Bootstrap

type state = {
	user: Client_auth.authenticated_user_state;
	db: Store.t;

	user_delete_error: string option;
	password_confirmation: string;

	preferences_error: string option;
	password_length_input: string;
	password_length: int option;
}

type internal_message = [
	| `db_changed of Store.t
	| `delete_account
	| `password_confirmation of string
	| `user_delete_error of string option
	| `preferences_error of string option
	| `save_preferences
	| `password_length of string
]

let string_of_message : internal_message -> string = function
	| `delete_account -> "`delete_account"
	| `password_confirmation p -> "`password_confirmation " ^ (mask_string p)
	| `user_delete_error error -> "`user_delete_error " ^ (Option.to_string quote_string error)
	| `preferences_error error -> "`preferences_error " ^ (Option.to_string quote_string error)
	| `save_preferences -> "`save_preferences"
	| `password_length len -> "`password_length " ^ len
	| `db_changed _ -> "`db_changed"

let string_of_state : state -> string = fun { user; _ } ->
	"{ user = " ^ (Client_auth.string_of_auth_state (user:>Client_auth.auth_state)) ^
	" }"

type message = [
	| internal_message
	| `hide
]

(* let password_change_section ~token ~close () = ( *)
(* 	let error, set_error = S.create None in *)
(* 	let error_widget = error *)
(* 		|> optional_signal_content (fun err -> Passe_ui.p ~cls:"text-danger" ~text:err ~children:[ *)
(* 			child i ~cls:"glyphicon glyphicon-remove" (); *)
(* 		] ()) *)
(* 		|> Passe_ui.stream in *)
(* 	let password_input ~label name = *)
(* 		let attrs = ["name",name; "type","password"] in *)
(* 		row `XS ~cls:"form-group" [ *)
(* 			col ~size:4 [control_label label]; *)
(* 			col [child input ~cls:"form-control" ~attrs:attrs ()]; *)
(* 		] *)
(* 	in *)
(*  *)
(* 	child div ~cls:"form-horizontal" ~children:[ *)
(* 		error_widget; *)
(* 		password_input ~label:"Old password" "new"; *)
(* 		password_input ~label:"New password" "new"; *)
(* 		password_input ~label:"New password (again)" "new2"; *)
(*  *)
(* 		row `XS [ *)
(* 			col ~size:8 ~offset:4 [ *)
(* 				child input ~cls:"btn btn-primary submit" ~attrs:[("type","submit"); ("value","Change password")] (); *)
(* 			] *)
(* 		]; *)
(* 	] ~mechanism:(fun form -> *)
(* 		let username = Client_auth.name_of_authenticated user in *)
(* 		let submit_button = form##querySelector(Js.string ".submit") |> non_null in *)
(* 		Lwt_js_events.clicks submit_button (fun event _ -> *)
(* 			stop event; *)
(* 			let pairs = get_form_contents form in *)
(* 			let data = `Assoc (pairs |> List.map (fun (a, b) -> a, `String b)) in *)
(* 			let new1 = data |> J.mandatory J.string_field "new" *)
(* 			and new2 = data |> J.mandatory J.string_field "new2" in *)
(* 			if new1 <> new2 then ( *)
(* 				set_error (Some "Passwords don't match"); *)
(* 				return_unit *)
(* 			) else ( *)
(* 				let open Server in *)
(* 				match_lwt post_json ~token ~data Client_auth.change_password_url with *)
(* 					| OK creds -> *)
(* 						Dom_html.window##alert (Js.string "Password changed."); *)
(* 						set_auth_state (`Active_user (username, creds)); *)
(* 						close (); *)
(* 						return_unit *)
(* 					| Unauthorized msg -> *)
(* 						set_error (Some (msg |> Option.default "Unauthorized")); *)
(* 						return_unit; *)
(* 					| Failed (_, msg,_) -> *)
(* 						set_error (Some msg); *)
(* 						return_unit *)
(* 			) *)
(* 		) *)
(* 	) () *)
(* ) in *)

let password_change_section () : message html = text "TODO: password change"

let current_password_length db = Store.((get_defaults db).default_length)

let preferences_section =
	let track_password_length = track_input_contents (fun str -> `password_length str) in

	fun { preferences_error; db; password_length; password_length_input; _ } -> (
		let error_widget = preferences_error
			|> Option.map (fun err ->
				p ~a:[a_class "text-danger"] [
					icon "remove";
					text err;
				]
			) |> Option.default empty
		in

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
			error_widget |> Ui.identify (`String "error");
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

let user_delete_section =
	let track_password_confirmation =
		track_input_contents (fun value -> `password_confirmation value)
	in
	fun { user_delete_error; password_confirmation; _ } -> (
		let error_widget = user_delete_error
			|> Option.map (fun err ->
				p ~a:[a_class "text-danger"] [
					i ~a:[a_class "glyphicon glyphicon-remove"] [];
					text err;
				]
		) |> Option.default (text "") in

		let password_field =
			input ~a:[
				a_input_type `Password;
				a_class "form-control";
				a_value password_confirmation;
				a_oninput track_password_confirmation;
			] ()
		in

		div ~a:[a_class "form-horizontal"] [
			error_widget |> Ui.identify (`String "error");
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

let view_panel _instance =
	let panel ~title children = Bootstrap.overlay ~cancel:`hide [Bootstrap.panel ~close:`hide ~title children] in
	(fun state ->
		panel ~title:"Account settings" ([
			preferences_section state;
			hr ()
		] @ (match state.user with
				| `Active_user _ | `Saved_user _ -> [
					password_change_section ();
					hr ();
					user_delete_section state;
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
		Ui.emit instance (`user_delete_error None);
		let open Server in
		let open Lwt in
		let return_error msg = Ui.emit instance (`user_delete_error (Some msg)); return_unit in
		Some (
			match_lwt post_json
				~token
				~data:(`Assoc ["password", `String password])
				Client_auth.delete_user_url
			with
				| OK _ ->
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

let save_preferences ~sync _instance { password_length; db; _ } = (
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

let panel_command sync instance =
	let set_auth_state = sync.Sync.set_auth_state in
	let delete_account = delete_account ~set_auth_state in
	(fun state msg -> match msg with
		| `delete_account ->
			Client_auth.token_of_authenticated state.user
				|> Option.bind (delete_account instance state.password_confirmation)
		| `hide -> Ui.emit instance (`password_confirmation ""); None
		| `save_preferences -> save_preferences ~sync instance state; None
		| _ -> None
	)

let panel sync : (state, message) Ui.component =
	Ui.component ~view:view_panel ~eq:(=) ~command:(panel_command sync) ()

let reset_preferences state =
	let current_length = current_password_length state.db in
	{ state with
		password_length_input = current_length |> string_of_int;
		password_length = None;
		preferences_error = None;
	}

type external_state = Store.t
let external_state sync : external_state React.signal = sync.Sync.db_fallback
let external_messages db = [ `db_changed db ]

let initial db auth_state =
	{
		user = auth_state;
		user_delete_error = None;
		preferences_error = None;
		password_confirmation = "";
		db;
		password_length_input = "";
		password_length = None;
	} |> reset_preferences

let update state = function
	| `password_confirmation password_confirmation -> { state with password_confirmation }
	| `user_delete_error user_delete_error -> { state with user_delete_error }
	| `preferences_error preferences_error -> { state with preferences_error }
	| `password_length password_length_input -> (
		let state length err = { state with
			password_length_input;
			password_length = length;
			preferences_error = err;
		} in
		match (try Some (int_of_string password_length_input) with _ -> None) with
			| None -> state None (Some "Not a number")
			| Some len -> state (Some len) None
	)
	(* command-only messages *)
	| `delete_account -> state
	| `save_preferences -> state
	| `db_changed db ->
			{ state with db } |> reset_preferences
