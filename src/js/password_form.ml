open Vdoml
open Passe
open Passe_js
(* open Lwt *)
(* open Js *)
(* open Dom_html *)
open Common
open React_ext
(* module List = List_ext *)
(* module J = Json_ext *)
module Log = (val Logging.log_module "password_form")

type input_target = [ `domain | `password ]
type domain_suggestions = {
	suggestions : string list;
	selected : int option;
}

type state = {
	domain : string;
	db : Store.t;
	master_password : string;
	generated_password : string option;
	active_input : input_target option;
	domain_suggestions : domain_suggestions option;
	domain_record : Store.domain option;
	(* clipboard_supported : bool; *)
	(* domain_is_active : bool; *)
	(* master_password : string; *)
	(* domain_record : Store.record; *)
	(* saved_domain_info : Store.record option; *)
}

let initial_state sync = {
	active_input = None;
	domain = "";
	master_password = "";
	domain_record = None;
	generated_password = None;
	db = sync.Sync.db_fallback |> S.value;
	domain_suggestions = None;
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
	{ state with domain_record = Store.lookup state.domain state.db }

let update state : [<message] -> state =
	let derive_domain_data state = state |> update_suggestions |> update_domain_record in
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

let view sync instance : state -> internal_message Html.html =
	let open Html in
	let open Bootstrap in
	let track_master_password = track_input_contents (fun text -> `master_password text) in
	let track_domain_text = track_input_contents (fun text -> `domain text) in
	let focus = a_dynamic "data-focus" (fun elem _attr ->
		Js.Opt.iter (Dom_html.CoerceTo.input elem) (fun elem -> elem##focus())
	) in
	let domain_keydown = Ui.bind instance (fun { domain_suggestions } e ->
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
	fun { domain; db; master_password; active_input; domain_suggestions } -> (

		let no_input_coersion = [
			a_attr "autocomplete" "off";
			a_attr "autocorrect" "off";
			a_attr "autocapitalize" "off";
		] in

		let track_focus dest = [
			a_onblur @@ emitter (`blur `domain);
			a_onfocus @@ emitter (`blur `domain);
		] in

		let domain_input = input ~a:([
			a_value domain;
			a_class_list (List.filter_map identity [
				Some "form-control";
				domain_suggestions |> Option.map (fun _ -> "suggestions");
			]);
			a_oninput track_domain_text;
			a_onkeydown (handler domain_keydown);
			a_name "domain";
			(if active_input = Some `domain then focus else None);
		] @ no_input_coersion @ track_focus `domain) () in

		let password_input = input ~a:([
			a_class "form-control password-input";
			a_input_type `Password;
			a_name "password";
			a_oninput track_master_password;
			a_value master_password;
			(if active_input = Some `password then focus else None);
		] @ no_input_coersion @ track_focus `password) () in

		(* let empty_domain = domain = "" in *)
		(* let domain_record = Store.lookup domain db in *)
		(* let saved_domain_info = match domain_record with *)
		(* 	| Some d -> d *)
		(* 	| None -> Store.default db domain *)
		(* in *)
		(* let domain_is_unknown = Option.is_none domain_record in *)

	(* 	let domain_info, update_domain_info = editable_signal saved_domain_info in *)
	(* 	let update_domain_info = fun v -> *)
	(* 		Log.info (fun m->m "updating domain info to %s" (Store.json_string_of_domain v)); *)
	(* 		update_domain_info v in *)
	(*  *)
	(*  *)
	(* 	(* build up the generated password. *)
	(* 	 * It tracks (master_password, domain_info), but only *)
	(* 	 * sampled on form_submits. Whenever the inputs change or *)
	(* 	 * clear_generated_password is triggered, the inputs are *)
	(* 	 * overridden to be `None`, so the password is removed. *)
	(* 	 *) *)
	(* 	let password_input_data = S.l2 (fun a b -> (a,b)) master_password domain_info in *)
	(* 	let password_resets, clear_generated_password = E.create () in *)
	(* 	let password_resets = *)
	(* 		(* reset password info to `None` when a reset is explicitly triggered, *)
	(* 		 * or when the input data changes *) *)
	(* 		let to_none = fun _ -> None in *)
	(* 		E.select [ *)
	(* 			password_resets |> E.map to_none; *)
	(* 			S.changes password_input_data |> E.map to_none; *)
	(* 		] in *)
	(*  *)
	(* 	let form_submits, submit_form = E.create () in *)
	(* 	let password_submissions = S.sample (fun () v -> v) form_submits password_input_data *)
	(* 		|> E.map (fun inputs -> (Some inputs)) in *)
	(*  *)
	(* 	let current_password = E.select [password_resets; password_submissions] *)
	(* 		|> S.hold None *)
	(* 		|> S.map (fun inputs -> inputs *)
	(* 			|> Option.map (fun (password, domain) *)
	(* 				-> Password.generate ~domain password *)
	(* 			) *)
	(* 		) *)
	(* 	in *)
	(*  *)
	(* 	let to_html_elem : Dom.node Js.t -> Dom_html.element Js.t Js.opt = fun node -> *)
	(* 		let elem = Dom.CoerceTo.element node in *)
	(* 		Opt.map elem Dom_html.element *)
	(* 	in *)
	(*  *)
	(* 	let upto_class cls (elem:#Dom_html.element Js.t) = *)
	(* 		let cls = Js.string cls in *)
	(* 		let rec up (elem:Dom_html.element Js.t) = *)
	(* 			if (elem##classList##contains(cls) |> Js.to_bool) *)
	(* 				then Opt.return elem *)
	(* 				else ( *)
	(* 					let elem = Opt.bind elem##parentNode to_html_elem in *)
	(* 					Opt.bind elem up *)
	(* 				) *)
	(* 		in *)
	(* 		up elem *)
	(* 	in *)

		let clear_btn ?right ?trigger target : internal_message html =

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

		let password_display = text "TODO" in
			(* current_password |> Passe_ui.optional_signal_content (fun (p:string) -> *)
	(* 		(* every time we display a new password, default to hidden *) *)
	(* 		set_show_plaintext_password false; *)
	(*  *)
	(* 		let length = String.length p in *)
	(*  *)
	(* 		let string_repeat s n = Array.fold_left (^) "" (Array.make n s) in *)
	(* 		let dummy_text = (string_repeat "•" length) in *)
	(* 		let dummy = span ~cls:"dummy" *)
	(* 			~children: [ *)
	(* 				frag (Passe_ui.text_stream (show_plaintext_password |> *)
	(* 				S.map (fun plain -> if plain then p else dummy_text))) *)
	(* 			] *)
	(* 			() *)
	(* 		in *)
	(* 		dummy#class_s "selected" is_selected; *)
	(*  *)
	(* 		let display = div *)
	(* 			~cls:"password-display" *)
	(* 			~mechanism:(fun elem -> *)
	(* 				let child = elem##querySelector(Js.string ".secret") |> non_null in *)
	(* 				let select () = *)
	(* 					Selection.select child; *)
	(* 					set_is_selected true *)
	(* 				in *)
	(* 				let update_highlight () = *)
	(* 					set_is_selected @@ Selection.is_fully_selected ~length child; *)
	(* 				in *)
	(*  *)
	(* 				set_select_generated_password select; *)
	(* 				select (); *)
	(*  *)
	(* 				effectful_stream_mechanism show_plaintext_password (fun _ -> select ()) <&> *)
	(* 				Lwt_js_events.clicks ~use_capture:true document (fun e _ -> *)
	(* 					update_highlight (); *)
	(* 					Lwt.return_unit *)
	(* 				) <&> *)
	(* 				Lwt_js_events.keyups ~use_capture:true document (fun e _ -> *)
	(* 					if e##shiftKey == Js._true then *)
	(* 						update_highlight (); *)
	(* 					Lwt.return_unit *)
	(* 				) <&> *)
	(* 				Lwt_js_events.clicks ~use_capture:false elem (fun e _ -> *)
	(* 					select (); *)
	(* 					Lwt.return_unit *)
	(* 				) *)
	(* 			) ~children:[ *)
	(* 				child span ~cls:"secret" ~text:p (); *)
	(* 				frag dummy; *)
	(* 			] () in *)
	(*  *)
	(* 		div ~cls:"popover static password-popover fade bottom in" ~attrs:["role","tooltip"] ~children:[ *)
	(* 			child div ~cls:"arrow" (); *)
	(* 			child div ~cls:"popover-content" ~children:[ *)
	(* 				child table ~children:[ *)
	(* 					child tr ~children:[ *)
	(*  *)
	(* 						child td ~children:[ *)
	(* 							child h4 ~cls:"title visible-lg visible-md" ~text:"Generated: " (); *)
	(* 						] (); *)
	(*  *)
	(* 						child td ~cls:"controls" ~children:[ *)
	(* 							child span ~cls:"toggle" *)
	(* 								~mechanism:plaintext_toggle_mech *)
	(* 								~children:[icon "eye-open"] (); *)
	(* 						] (); *)
	(*  *)
	(* 						child td ~attrs:["width","*"] ~children:[ *)
	(* 							frag display; *)
	(* 						] (); *)
	(*  *)
	(* 						(clipboard_supported |> S.map (fun supported -> *)
	(* 							if supported then *)
	(* 								Some (td ~cls:"controls" ~children:[ *)
	(* 									child span ~cls:"toggle" *)
	(* 										~mechanism:copy_generated_password_mech *)
	(* 										~children:[icon "paperclip"] (); *)
	(* 								] ()) *)
	(* 							else *)
	(* 								None *)
	(* 						) |> Passe_ui.option_stream); *)
	(*  *)
	(* 						child td ~cls:"controls" ~children:[ *)
	(* 							child span ~cls:"toggle" *)
	(* 								~mechanism:clear_generated_password_mech *)
	(* 								~children:[icon "remove"] (); *)
	(* 						] (); *)
	(* 					] (); *)
	(* 				] (); *)
	(* 			] (); *)
	(* 		] (); *)
	(* 	) |> Passe_ui.stream in *)
	(*  *)
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
	(*  *)
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

	(* 	form#mechanism (fun elem -> *)
	(* 		let submit_button = elem##querySelector(Js.string ".submit") |> non_null in *)
	(* 		let password_input = elem##querySelector(Js.string "input.password-input") |> non_null in *)
	(* 		let clear_password () = *)
	(* 			set_master_password ""; *)
	(* 			let input = elem##querySelector (Js.string "input[name=password]") in *)
	(* 			let input = Opt.bind input Dom_html.CoerceTo.input in *)
	(* 			let input = Opt.get input (fun () -> failwith "can't find password input") in *)
	(* 			input##focus (); *)
	(* 		in *)
	(*  *)
	(* 		let submit_form event = *)
	(* 			Passe_ui.stop event; *)
	(* 			Log.info (fun m->m "form submitted"); *)
	(* 			elem##querySelectorAll(s"input") |> Dom.list_of_nodeList *)
	(* 				|> List.iter (fun elem -> *)
	(* 						let input = CoerceTo.input(elem) |> non_null in *)
	(* 						input##blur() *)
	(* 				); *)
	(* 			submit_form (); *)
	(* 			Lwt.return_unit *)
	(* 		in *)
	(*  *)
	(* 		Lwt_js_events.keydowns ~use_capture:true elem (fun event _ -> *)
	(* 			if (to_bool event##ctrlKey && event##keyCode = Keycode.s) then ( *)
	(* 				stop event; *)
	(* 				save_current_domain (); *)
	(* 				return_unit *)
	(* 			) else ( *)
	(* 				return_unit *)
	(* 			) *)
	(* 		) *)
	(* 		<&> *)
	(* 		Lwt_js_events.keydowns ~use_capture:false document (fun event _ -> *)
	(* 			if (event##keyCode = Keycode.esc) then ( *)
	(* 				stop event; *)
	(* 				clear_password () *)
	(* 			); *)
	(* 			return_unit *)
	(* 		) *)
	(* 		<&> *)
	(* 		Lwt_js_events.clicks submit_button (fun event _ -> submit_form event) *)
	(* 		<&> *)
	(* 		Lwt_js_events.keydowns password_input (fun event _ -> *)
	(* 			if event##keyCode = Keycode.return then ( *)
	(* 				submit_form event *)
	(* 			) else return_unit *)
	(* 		) *)
	(* 		<&> *)
	(* 		(while_lwt true do *)
	(* 			lwt () = Lwt_react.E.next form_submits in *)
	(* 			Lwt.pick [ *)
	(* 				( *)
	(* 					lwt _ = Lwt_react.E.next (S.changes password_input_data) in *)
	(* 					Log.debug (fun m->m "input changed; cancelling timeout"); *)
	(* 					return_unit *)
	(* 				); *)
	(* 				( *)
	(*  *)
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
		form
	)

let component sync = Ui.component ~view:(view sync) ()
