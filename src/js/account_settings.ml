open Passe
open Passe_js
open Common
module Log = (val Logging.log_module "account_settings")

open Vdoml
open Html
open Bootstrap

type state = {
	error: string option;
	user: Client_auth.authenticated_user_state;
	password_confirmation: string;
}

type internal_message = [
	| `delete_account
	| `password_confirmation of string
	| `error of string option
]

let string_of_message : internal_message -> string = function
	| `delete_account -> "`delete_account"
	| `password_confirmation p -> "`password_confirmation " ^ (String.make (String.length p) '*')
	| `error error -> "`error " ^ (Option.to_string quote_string error)

type message = [
	| internal_message
	| `hide
]

(* let account_settings_button instance = ( *)
(* let preferences_section ~close () = ( *)
(* 	let error, set_error = S.create None in *)
(* 	let error_widget = error *)
(* 		|> optional_signal_content (fun err -> Passe_ui.p ~cls:"text-danger" ~text:err ~children:[ *)
(* 			icon "remove"; *)
(* 		] ()) *)
(* 		|> Passe_ui.stream in *)
(* 	let db = S.value state.db_fallback in *)
(* 	let current_length = Store.((get_defaults db).default_length) in *)
(* 	let length, set_length = S.create current_length in *)
(* 	let set_length str = *)
(* 		match (try Some (int_of_string str) with _ -> None) with *)
(* 			| Some len -> set_length len *)
(* 			| None -> set_error (Some "Not a number") *)
(* 	in *)
(* 	let length_field = input_of_signal ~update:set_length (S.map string_of_int length) in *)
(* 	length_field#attr "class" "form-control"; *)
(*  *)
(* 	child div ~cls:"form-horizontal" ~children:[ *)
(* 		error_widget; *)
(*  *)
(* 		row `XS ~cls:"form-group" [ *)
(* 			col ~size:4 [control_label "Default password length"]; *)
(* 			col [frag length_field]; *)
(* 		]; *)
(*  *)
(* 		row `XS [ *)
(* 			col ~size:8 ~offset:4 [ *)
(* 				child input ~cls:"btn btn-primary save" ~attrs:[("type","submit"); ("value","Save defaults")] (); *)
(* 			]; *)
(* 		]; *)
(* 	] ~mechanism:(fun form -> *)
(* 		let submit_button = form##querySelector(Js.string ".save") |> non_null in *)
(* 		Lwt_js_events.clicks submit_button (fun event _ -> *)
(* 			stop event; *)
(* 			let length = S.value length in *)
(* 			let saved = if length = current_length *)
(* 				then true *)
(* 				else Sync.save_default ~state (`Length length) *)
(* 			in *)
(* 			if saved then ( *)
(* 				Dom_html.window##alert (Js.string "Defaults saved"); *)
(* 				close (); *)
(* 			) else ( *)
(* 				Dom_html.window##alert (Js.string "Unable to save DB"); *)
(* 			); *)
(* 			return_unit *)
(* 		) *)
(* 	) (); *)
(* ) in *)
(*  *)
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
(*  *)
(* ) in *)

let password_change_section () : message html = text "TODO: password change"
let preferences_section () : message html = text "TODO: preferences change"

let user_delete_section =
	let track_password_confirmation =
		track_input_contents (fun value -> `password_confirmation value)
	in
	fun { error; password_confirmation } -> (
		let error_widget = error
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
			error_widget;
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

let view_panel instance state =
	Bootstrap.overlay ~cancel:`hide [
		Bootstrap.panel ~title:"Account settings" ([
			preferences_section ();
			hr ()
		] @ (match state.user with
				| `Active_user (_, token)  | `Saved_user (_, token) -> [
					password_change_section ();
					hr ();
					user_delete_section state;
				]
				| `Implicit_user _ | `Saved_implicit_user _ -> []
			)
		)
	]

let button : (unit, unit) Ui.component = Ui.component
	~view:(fun instance state -> a ~a:[
		a_class "hover-reveal link settings-button";
		a_title "Settings";
		a_onclick (emitter ());
	] [icon "cog"])
	()

let delete_account ~set_auth_state instance password token = (
	if (Dom_html.window##confirm (Js.string "Are you SURE?") |> Js.to_bool) then (
		Ui.emit instance (`error None);
		let open Server in
		let open Lwt in
		let return_error msg = Ui.emit instance (`error (Some msg)); return_unit in
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

let panel_command sync =
	let set_auth_state = sync.Sync.set_auth_state in
	let delete_account = delete_account ~set_auth_state in
	(fun instance state msg -> match msg with
		| `delete_account ->
			Client_auth.token_of_authenticated state.user
				|> Option.bind (delete_account instance state.password_confirmation)
		| `hide -> Ui.emit instance (`password_confirmation ""); None
		| _ -> None
	)

let panel sync : (state, message) Ui.component =
	Ui.component ~view:view_panel ~command:(panel_command sync) ()

let initial auth_state = {
	user = auth_state;
	error = None;
	password_confirmation = "";
}

let update state = function
	| `delete_account -> state
	| `password_confirmation password_confirmation -> { state with password_confirmation }
	| `error error -> { state with error }

