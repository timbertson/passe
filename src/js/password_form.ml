open Vdoml
open Passe
open Passe_js
open Common
open React_ext
module Log = (val Logging.log_module "password_form")

type input_target = [ `domain | `password ]
type domain_suggestions = {
	suggestions : string list;
	selected : int option;
}

type generated_password = {
	password: string;
	visible: bool;
	fully_selected: bool;
}

type state = {
	domain : string;
	domain_record : Store.domain;
	saved_domain_record : Store.domain option;
	db : Store.t;
	master_password : string;
	generated_password : generated_password option;
	active_input : input_target option;
	domain_suggestions : domain_suggestions option;
	clipboard_supported : bool;
}

let eq a b =
	let {
		domain = domain_a;
		db = db_a;
		master_password = master_password_a;
		generated_password = generated_password_a;
		active_input = active_input_a;
		domain_suggestions = domain_suggestions_a;
		domain_record = domain_record_a;
		saved_domain_record = saved_domain_record_a;
		clipboard_supported = clipboard_supported_a;
	} = a in
	let {
		domain = domain_b;
		db = db_b;
		master_password = master_password_b;
		generated_password = generated_password_b;
		active_input = active_input_b;
		domain_suggestions = domain_suggestions_b;
		domain_record = domain_record_b;
		saved_domain_record = saved_domain_record_b;
		clipboard_supported = clipboard_supported_b;
	} = b in
	(
		db_a == db_b && (* don't bother deep equality checking *)
		domain_a = domain_b &&
		master_password_a = master_password_b &&
		generated_password_a = generated_password_b &&
		active_input_a = active_input_b &&
		domain_suggestions_a = domain_suggestions_b &&
		domain_record_a = domain_record_b &&
		saved_domain_record_a = saved_domain_record_b &&
		clipboard_supported_a = clipboard_supported_b
	)

let initial_state sync =
	let db = sync.Sync.db_fallback |> S.value in
	let domain = "" in
	{
		db;
		domain;
		domain_record = Store.default db domain;
		saved_domain_record = None;
		master_password = "";
		generated_password = None;
		domain_suggestions = None;
		active_input = None;
		clipboard_supported = true;
	}

type direction = [ `up | `down ]
type internal_message = [
	| `master_password of string
	| `domain of string
	| `clear of input_target
	| `blur of input_target
	| `accept_suggestion of string
	| `select_cursor of direction
	| `select_suggestion of int option
	| `password_fully_selected of bool
	| `clipboard_supported of bool
	| `toggle_password_visibility
	| `generate_password
]

type external_state_notification = [
	| `db_changed of Store.t
]

type message = [
	| internal_message
	| external_state_notification
]

let string_of_message : [<message] -> string =
	let string_of_target = function `domain -> "`domain" | `password -> "`password" in
	function
	| `master_password password -> "`master_password " ^ (mask_string password)
	| `domain domain -> "`domain " ^ domain
	| `clear target -> "`clear " ^ (string_of_target target)
	| `blur target -> "`blur " ^ (string_of_target target)
	| `db_changed _ -> "`db_changed"
	| `accept_suggestion text -> "`accept_suggestion " ^ text
	| `select_cursor direction -> "`select_cursor " ^ (match direction with `up -> "up" | `down -> "down")
	| `select_suggestion idx -> "`select_suggestion " ^ (Option.to_string string_of_int idx)
	| `password_fully_selected sel -> "`password_fully_selected " ^ (string_of_bool sel)
	| `toggle_password_visibility -> "`toggle_password_visibility"
	| `generate_password -> "`generate_password"
	| `clipboard_supported supported -> "`clipboard_supported " ^ (string_of_bool supported)

let update_suggestions state =
	let s = match state.active_input with
		| Some `domain -> if state.domain = "" then None else (
			Store.keys_like state.db state.domain
			|> List.filter ((<>) state.domain)
			|> Option.non_empty ~zero:[]
		)
		| _ -> None
	in
	{ state with domain_suggestions = s |> Option.map (fun suggestions -> { suggestions; selected = None }) }

let update_domain_record state =
	let saved_domain_record = Store.lookup state.domain state.db in
	let domain_record = saved_domain_record |> Option.default_fn
		(fun () -> Store.default state.db state.domain) in
	{ state with domain_record; saved_domain_record }

let update state : [<message] -> state =
	let derive_domain_data state = state |> update_suggestions |> update_domain_record in
	let modify_generated_password state fn =
		{ state with generated_password = state.generated_password |> Option.map fn }
	in
	function
	| `clear dest -> (
		let state = { state with generated_password = None; active_input = Some dest } in
		match dest with
			| `domain -> { state with domain = "" }
			| `password -> { state with master_password = "" }
		)
	| `blur dest ->
			if state.active_input = Some dest
				then { state with active_input = None }
				else state
	| `domain domain -> { state with domain; active_input = Some `domain } |> derive_domain_data
	| `db_changed db -> { state with db } |> derive_domain_data
	| `master_password master_password -> { state with master_password }
	| `select_cursor direction ->
			let domain_suggestions = state.domain_suggestions |> Option.map (fun suggestions ->
				let idx = suggestions.selected |> Option.default (-1) in
				let idx = match direction with
					| `up -> min ((List.length suggestions.suggestions) - 1) (idx + 1)
					| `down -> max (-1) (idx - 1)
				in
				{ suggestions with selected = idx |> Option.non_empty ~zero:-1 }
			) in
			{ state with domain_suggestions }
	| `accept_suggestion domain ->
			{ state with
				domain;
				active_input = Some `password;
				domain_suggestions = None;
			} |> update_domain_record
	| `select_suggestion selected ->
		let domain_suggestions = state.domain_suggestions |> Option.map (fun suggestions ->
			{ suggestions with selected }
		) in
		{ state with domain_suggestions } |> update_domain_record
	| `password_fully_selected fully_selected ->
		modify_generated_password state (fun password ->
			{ password with fully_selected }
		)
	| `toggle_password_visibility -> 
		modify_generated_password state (fun password ->
			{ password with visible = not password.visible }
		)
	| `generate_password ->
		{ state with generated_password = Some {
			password = Password.generate ~domain:state.domain_record state.master_password;
			visible = false;
			fully_selected = true;
		}}
	| `clipboard_supported clipboard_supported -> { state with clipboard_supported }

let id_counter = ref 0
let view instance : state -> internal_message Html.html =
	let open Html in
	let open Bootstrap in
	let instance_id = string_of_int !id_counter in
	let password_element_id = "password-contents-"^ instance_id in
	incr id_counter;
	let track_master_password = track_input_contents (fun text -> `master_password text) in
	let track_domain_text = track_input_contents (fun text -> `domain text) in
	let submit_on_return = handler (fun e ->
		e |> Event.keyboard_event |> Option.bind (fun e ->
			if e##keyCode = Keycode.return
				then Some (Event.handle `generate_password)
				else None
		) |> Event.optional
	) in
	let focus = a_dynamic "data-focus" (fun elem _attr ->
		Js.Opt.iter (Dom_html.CoerceTo.input elem) (fun elem -> elem##focus())
	) in
	let all_text_selected = a_dynamic "data-selected" (fun elem _attr ->
		Selection.select elem
	) in
	let update_selection = handler @@ Ui.bind instance (fun { generated_password; _ } e ->
		generated_password |> Option.bind (fun { password; _ } ->
			e##target |> Js.Opt.to_option |> Option.map (fun target ->
				let is_selected = Selection.is_fully_selected ~length:(String.length password) target in
				Event.return `Unhandled (`password_fully_selected is_selected)
			)
		) |> Event.optional
	) in
	let select_password_text = handler @@ fun _ ->
		Event.handle (`password_fully_selected true)
	in
	let domain_keydown = handler @@ Ui.bind instance (fun { domain_suggestions; _ } e ->
		let open Js in
		e |> Event.keyboard_event |> Option.bind (fun e ->
			if e##keyCode = Keycode.esc then (
				Some (Event.handle (`domain ""))
			) else (
				e##keyIdentifier
				|> Optdef.to_option
				|> Option.or_fn (fun () -> Unsafe.get e "key" |> Optdef.to_option)
				|> Option.bind (fun ident ->
					match Js.to_string ident with
					| "Up" | "ArrowUp" -> Some (Event.handle (`select_cursor `up))
					| "Down" | "ArrowDown" -> Some (Event.handle (`select_cursor `down))
					| _ -> (
						let which = Unsafe.get e "which" in
						let select_current () =
							domain_suggestions |> Option.bind (fun {suggestions; selected} ->
								selected |> Option.bind (fun idx ->
									try
										let result = `accept_suggestion (List.nth suggestions idx) in
										Log.debug (fun m->m "selecting item %d" idx);
										Some (Event.handle result)
									with Not_found -> (
										Log.debug (fun m->m "can't select (no such item): %d" idx);
										None
									)
								)
							)
						in
						Log.debug (fun m->m "Processing key code %d" which);
						begin match which with
							| k when k = Keycode.tab -> select_current ()
							| k when k = Keycode.return -> select_current ()
							| k -> Log.debug (fun m->m "ignoring unknown key code %d" k); None
						end
					)
				)
			)
		) |> Event.optional
	) in

	let get_password_element () =
		let sel = "#"^password_element_id in
		let found = Dom_html.document##documentElement##querySelector(Js.string sel)
			|> Js.Opt.to_option in
		if Option.is_none found then (
			Log.warn (fun m->m "Error locating password element %s" sel)
		);
		found
	in

	let update_highlight () =
		get_password_element () |> Option.may (fun elem ->
			Ui.emit instance (`password_fully_selected (Selection.is_fully_selected elem))
		)
	in

	let copy_generated_password = handler (fun _ ->
		get_password_element () |> Option.map (fun elem ->
			Selection.select elem;
			Ui.emit instance (`password_fully_selected true);
			match Clipboard.triggerCopy () with
				| Some error ->
					Log.err (fun m->m "%s" error);
					Event.handle (`clipboard_supported false)
				| None ->
					(* give a bit of UI feedback in the default case that the
					 * selection has been copied *)
					Event.handle (`clear `password)
		) |> Event.optional
	) in

	let root_hooks =
		let events = ref [] in
		let create _ =
			let open Dom_html in
			let doc = Dom_html.document##documentElement in
			events := !events @ [
				addEventListener doc Dom_html.Event.click (handler (fun _ ->
					update_highlight ();
					Js._true (* continue event *)
				)) Js._true (* use capture *)
				;
				addEventListener doc Dom_html.Event.keydown (handler (fun _ ->
					Ui.emit instance (`clear `password);
					Js._false (* stop event *)
				)) Js._false (* use capture *);
			]
		in
		let destroy _elem =
			!events |> List.iter (Dom_html.removeEventListener);
			events := []
		in
		fun node -> Ui.hook ~create ~destroy node
	in

	(fun state ->
		let { domain; master_password; active_input; domain_suggestions; _ } = state in
		let no_input_coersion = [
			a_attr "autocomplete" "off";
			a_attr "autocorrect" "off";
			a_attr "autocapitalize" "off";
		] in

		let track_focus dest = [
			a_onblur @@ emitter (`blur dest);
			a_onfocus @@ emitter (`blur dest);
		] in

		let domain_input = input ~a:([
			a_value domain;
			a_class_list (List.filter_map identity [
				Some "form-control";
				domain_suggestions |> Option.map (fun _ -> "suggestions");
			]);
			a_oninput track_domain_text;
			a_onkeydown domain_keydown;
			a_name "domain";
			(if active_input = Some `domain then focus else None);
		] @ no_input_coersion @ track_focus `domain) () in

		let password_input = input ~a:([
			a_class "form-control password-input";
			a_input_type `Password;
			a_name "password";
			a_oninput track_master_password;
			a_onkeydown submit_on_return;
			a_value master_password;
			(if active_input = Some `password then focus else None);
		] @ no_input_coersion @ track_focus `password) () in

		let clear_btn ?right target : internal_message html =

			span ~a:[
				a_class "link text-muted clear-btn";
				a_style ("right:"^(right |> Option.default 25 |> string_of_int)^"px;");
				a_onclick @@ emitter (`clear target);
			] [icon "remove"]
		in

		let make_suggestion_ui : domain_suggestions option -> internal_message html = function
			| None -> empty
			| Some { suggestions; selected } ->
				ul ~a:[
					a_class "suggestions";
					a_onmouseout @@ emitter (`select_suggestion None);
				] (
					suggestions |> List.mapi (fun i text ->
						li ~a:[
							(if Some i = selected then a_class "selected" else None);
							a_onmousedown (emitter (`accept_suggestion text));
							a_onmouseover (emitter (`select_suggestion (Some i)));
						] [Html.text text]
					)
				)
		in

	(* 	let open Store in *)
	(* 	let domain_info_editor ~get ~set = input_of_signal *)
	(* 		~update:(fun v -> update_domain_info (set v)) *)
	(* 		(saved_domain_info |> S.map get) *)
	(* 	in *)
	(* 	 *)
	(* 	let notes_input = domain_info_editor *)
	(* 		~get:(fun d -> d.note |> default_empty_string) *)
	(* 		~set:(fun v -> {S.value saved_domain_info with note=non_empty_string v}) in *)
	(*  *)
	(* 	let length_input = domain_info_editor *)
	(* 		~get:(fun d -> d.length |> string_of_int) *)
	(* 		~set:(fun v -> {S.value saved_domain_info with length=int_of_string v}) in *)
	(*  *)
	(* 	let suffix_input = domain_info_editor *)
	(* 		~get:(fun d -> d.suffix |> default_empty_string) *)
	(* 		~set:(fun v -> {S.value saved_domain_info with suffix=non_empty_string v}) in *)
	(*  *)
	(* 	let show_plaintext_password, set_show_plaintext_password = S.create false in *)
	(*  *)
	(* 	let is_selected, set_is_selected = S.create true in *)
	(* 	let deselect () = Selection.deselect (); set_is_selected false in *)
	(*  *)
	(* 	let select_generated_password, set_select_generated_password = S.create (fun () -> *)
	(* 		raise (AssertionError "select_generated_password not yet set") *)
	(* 	) in *)
	(*  *)
	(* 	let click_action fn = (fun elem -> *)
	(* 		Lwt_js_events.clicks ~use_capture:true elem (fun e _ -> *)
	(* 			Passe_ui.stop e; *)
	(* 			fn elem e; *)
	(* 			return_unit *)
	(* 		) *)
	(* 	) in *)
	(*  *)
	(* 	let plaintext_toggle_mech = click_action (fun _ _ -> *)
	(* 		set_show_plaintext_password (not @@ S.value show_plaintext_password); *)
	(* 	) in *)
	(*  *)
	(* 	let clear_generated_password_mech = click_action (fun _ _ -> *)
	(* 		clear_generated_password (); *)
	(* 	) in *)
	(*  *)
	(* 	let copy_generated_password_mech = click_action (fun _ _ -> *)
	(* 		(S.value select_generated_password) (); *)
	(* 		match Clipboard.triggerCopy () with *)
	(* 			| Some error -> *)
	(* 				Log.err (fun m->m "%s" error); *)
	(* 				set_clipboard_supported false *)
	(* 			| None -> *)
	(* 				(* give a bit of UI feedback in the default case that the *)
	(* 				 * selection has been copied *) *)
	(* 				deselect () *)
	(* 	) in *)

		let password_display = state.generated_password |> Option.map (fun { password; visible; fully_selected } ->
			let password_display = if visible then password else (
				let length = String.length password in
				let string_repeat s n = Array.fold_left (^) "" (Array.make n s) in
				(string_repeat "•" length)
			) in

			let display = div ~a:[a_class "password-display"] [
				span ~a:[
					a_class "secret";
					a_id password_element_id;
					if fully_selected then all_text_selected else None;
					a_onclick select_password_text;
					a_onmouseup update_selection;
					a_onkeyup update_selection;
				] [text password];
				span ~a:[
					a_class_list (
						[ "dummy" ] @ (if fully_selected then ["selected"] else [])
					);
				] [ text password_display ];
			] in

			div ~a:[
				a_class "popover static password-popover fade bottom in";
				a_role "tooltip";
			] [
				div ~a:[a_class "arrow"] [];
				div ~a:[a_class "popover-content"] [
					table [
						tr [
							td [
								h4 ~a:[a_class "title visible-lg visible-md"] [text "Generated: "];
							];

							td ~a:[a_class "controls"] [
								span ~a:[
									a_class "toggle"; a_onclick (emitter `toggle_password_visibility)
								] [icon "eye-open"];
							];

							td ~a:[a_width "*"] [ display ];

							(if state.clipboard_supported
								then td ~a:[a_class "controls"] [
									span ~a:[
										a_class "toggle";
										a_onclick copy_generated_password;
									] [icon "paperclip"];
								]
								else empty
							);

							td ~a:[a_class "controls"] [
								span ~a:[
									a_class "toggle";
									a_onclick (emitter (`clear `password));
								] [icon "remove"];
							];
						];
					];
				];
			]
		) |> Option.default empty in
	(* 	let unchanged_domain = S.l2 (fun db_dom domain_info -> *)
	(* 		match db_dom with *)
	(* 			| Some db -> Store.record_eq (Domain db) (Domain domain_info) *)
	(* 			| None -> false *)
	(* 	) domain_record domain_info in *)
	(*  *)
	(* 	let save_current_domain () = *)
	(* 		let (_saved:bool) = Sync.save_change ~state:sync *)
	(* 			~original:(S.value domain_record |> Option.map (fun d -> Domain d)) *)
	(* 			(Some (Domain (S.value domain_info))) in *)
	(* 		() *)
	(* 	in *)

		let domain_panel = text "TODO" in
	(* 	let domain_panel = Passe_ui.div ~cls:"domain-info panel" () in *)
	(* 	let () = *)
	(* 		let open Store in *)
	(* 		let open Passe_ui in *)
	(* 		domain_panel#class_s "unknown" domain_is_unknown; *)
	(* 		domain_panel#class_s "hidden" empty_domain; *)
	(*  *)
	(* 		let no_user = sync.Sync.current_uid |> S.map Option.is_none in *)
	(* 		let save_button = input ~attrs:[("type","button");("value","save");("title","(ctrl+s)")] () in *)
	(* 		save_button#class_s "hidden" (S.l2 (||) no_user unchanged_domain); *)
	(*  *)
	(* 		save_button#mechanism (fun elem -> *)
	(* 			Lwt_js_events.clicks elem (fun event _ -> *)
	(* 				Passe_ui.stop event; *)
	(* 				save_current_domain (); *)
	(* 				return_unit *)
	(* 			) *)
	(* 		); *)
	(*  *)
	(* 		let delete_button = a ~cls:"delete link" ~children:[icon "remove"] () in *)
	(* 		delete_button#class_s "hidden" (S.l2 (||) no_user domain_is_unknown); *)
	(* 		delete_button#mechanism (fun elem -> *)
	(* 			Lwt_js_events.clicks elem (fun event _ -> *)
	(* 				stop event; *)
	(* 				let (_saved:bool) = Sync.save_change ~state:sync *)
	(* 					~original:(S.value domain_record |> Option.map (fun d -> Domain d)) *)
	(* 					None in *)
	(* 				return_unit *)
	(* 			) *)
	(* 		); *)
	(*  *)
	(* 		let inline_input text input = *)
	(* 			child div ~cls:"inline" ~children:[ *)
	(* 				child strong ~text: text (); *)
	(* 				child span ~children:[ *)
	(* 					frag input; *)
	(* 				] (); *)
	(* 			] () *)
	(* 		in *)
	(*  *)
	(*  *)
	(* 		domain_panel#append_all [ *)
	(* 			child div ~cls:"panel-heading" ~children:[ *)
	(* 				child h3 ~children: [ *)
	(* 					frag delete_button; *)
	(* 					(S.l3 (fun domain unknown unchanged -> *)
	(* 						if unknown || unchanged *)
	(* 							then domain *)
	(* 							else domain^" *") *)
	(* 						domain domain_is_unknown unchanged_domain *)
	(* 					) |> Passe_ui.text_stream; *)
	(*  *)
	(* 					frag save_button; *)
	(* 				] (); *)
	(* 			] (); *)
	(* 			child div ~cls:"panel-body" ~children: [ *)
	(* 				inline_input "Length:" length_input; *)
	(* 				inline_input "Notes:" notes_input; *)
	(* 				inline_input "Suffix:" suffix_input; *)
	(* 			] (); *)
	(* 		]; *)
	(* 	in *)

		let form =
			let left elem = col ~size:2 [elem] in
			div ~a:[a_class "form-horizontal main-form"; a_role "form"] [
			row `Sml [
				col ~size:7 ~cls:"password-form" [
					row `XS ~collapse:true ~cls:"form-group" [
						left @@ control_label "Domain";
						col [
							clear_btn `domain;
							domain_input;
							make_suggestion_ui domain_suggestions;
						];
					];

					row `XS ~collapse:true ~cls:"form-group" [
						left @@ control_label "Password";
						col [
							clear_btn ~right:68 `password;
							div ~a:[a_class "input-group"] [
								password_input;
								span ~a:[a_class "input-group-btn"] [
									button ~a:[
										a_class "btn btn-default submit";
										a_input_type `Submit;
										a_onclick (emitter `generate_password);
									] [icon "play"];
								];
							];
							password_display;
						];
					];
				];

				col [domain_panel];
			];
		] in

	(* 		Lwt_js_events.keydowns ~use_capture:true elem (fun event _ -> *)
	(* 			if (to_bool event##ctrlKey && event##keyCode = Keycode.s) then ( *)
	(* 				stop event; *)
	(* 				save_current_domain (); *)
	(* 				return_unit *)
	(* 			) else ( *)
	(* 				return_unit *)
	(* 			) *)
	(* 		) *)
	(* 					(* After generating a password, if the window stays blurred *)
	(* 					 * for more than a few seconds we clear the master password (to prevent *)
	(* 					 * leaving master passwords around) *)
	(* 					 *) *)
	(* 					let await evt subject : unit Lwt.t = *)
	(* 						lwt _ = evt subject in *)
	(* 						return_unit *)
	(* 					in *)
	(* 					let blur_timeout = 10.0 in *)
	(* 					while_lwt true do *)
	(* 						Log.info (fun m->m "awaiting window blur"); *)
	(* 						lwt () = await Lwt_js_events.blur window in *)
	(* 						Lwt.pick [ *)
	(* 							( *)
	(* 								lwt () = await Lwt_js_events.focus window in *)
	(* 								Log.info (fun m->m "window back in focus"); *)
	(* 								(* continue loop, waiting until next blur *) *)
	(* 								return_unit *)
	(* 							); *)
	(* 							( *)
	(* 								Log.info (fun m->m "window blurred; clearing pwd in %fs" blur_timeout); *)
	(* 								lwt () = Lwt_js.sleep blur_timeout in *)
	(* 								Log.info (fun m->m "clearing generated password"); *)
	(* 								(* cancels this branch because the other one is waiting *)
	(* 								 * on `password_input_data` changes *) *)
	(* 								clear_password (); *)
	(* 								return_unit *)
	(* 							) *)
	(* 						] *)
	(* 					done *)
	(* 				); *)
	(* 			] *)
	(* 		done) *)
	(*  *)
	(* 	); *)
		form |> root_hooks
	)

let component = Ui.component ~eq ~view ()
