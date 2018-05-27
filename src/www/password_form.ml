open Vdoml
open Passe
open Passe_js
open Common
open React_ext
open Js_util
module Log = (val Logging.log_module "password_form")
module Keycode = Keycode_ext

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
	db : Store.t;
	master_password : string;
	generated_password : generated_password option;
	active_input : input_target option;
	domain_suggestions : domain_suggestions option;
	clipboard_supported : bool;
	domain_form : Domain_form.state;
}

let string_of_generated_password { password; visible; fully_selected } =
	"{ password = " ^ (mask_string password) ^
	"; visible = " ^ (string_of_bool visible) ^
	"; fully_selected = " ^ (string_of_bool fully_selected) ^
	"}"

let string_of_target = function
	| `domain -> "`domain"
	| `password -> "`password"

let string_of_state state =
	"{ domain = " ^ (quote_string state.domain) ^
	"; db = (...)" ^
	"; domain_suggestions = (...)" ^
	"; master_password = " ^ (mask_string state.master_password) ^
	"; generated_password = " ^ (Option.to_string string_of_generated_password state.generated_password) ^
	"; active_input = " ^ (Option.to_string string_of_target state.active_input) ^
	"; clipboard_supported = " ^ (string_of_bool state.clipboard_supported) ^
	"; domain_form = " ^ (Domain_form.string_of_state state.domain_form) ^
	"}"

let eq a b =
	let {
		domain = domain_a;
		db = db_a;
		master_password = master_password_a;
		generated_password = generated_password_a;
		active_input = active_input_a;
		domain_suggestions = domain_suggestions_a;
		clipboard_supported = clipboard_supported_a;
		domain_form = domain_form_a;
	} = a in
	let {
		domain = domain_b;
		db = db_b;
		master_password = master_password_b;
		generated_password = generated_password_b;
		active_input = active_input_b;
		domain_suggestions = domain_suggestions_b;
		clipboard_supported = clipboard_supported_b;
		domain_form = domain_form_b;
	} = b in
	(
		db_a == db_b && (* don't bother deep equality checking *)
		domain_a = domain_b &&
		master_password_a = master_password_b &&
		generated_password_a = generated_password_b &&
		active_input_a = active_input_b &&
		domain_suggestions_a = domain_suggestions_b &&
		clipboard_supported_a = clipboard_supported_b &&
		Domain_form.eq domain_form_a domain_form_b
	)

let domain_form_message m = `domain_form m

type external_state = Store.t * Domain_form.external_state
let external_state sync : external_state React.signal = let open Sync in S.l2 (fun a b -> a,b)
	sync.db_fallback
	(Domain_form.external_state sync)

let external_messages (db, domain_form) = [ `db_changed db ] @
	(Domain_form.external_messages domain_form |> List.map domain_form_message)

let initial ((db, domain_form_state):external_state) domain_text =
	{
		db;
		domain = domain_text;
		domain_form = Domain_form.initial domain_form_state domain_text;
		master_password = "";
		generated_password = None;
		domain_suggestions = None;
		active_input = None;
		clipboard_supported = true;
	}

type direction = [ `up | `down ]

type message = [
	| `master_password of string
	| `domain of string
	| `clear of input_target
	| `cancel
	| `track_blur of input_target
	| `track_focus of input_target
	| `window_blur
	| `window_focus
	| `accept_suggestion of string
	| `select_cursor of direction
	| `select_suggestion of int option
	| `password_fully_selected of bool
	| `clipboard_supported of bool
	| `toggle_password_visibility
	| `generate_password
	| `domain_form of Domain_form.message
	| `db_changed of Store.t
]

let string_of_direction : direction -> string = function
	| `up -> "`up"
	| `down -> "`down"

let string_of_message : message -> string = function
	| `master_password password -> "`master_password " ^ (mask_string password)
	| `domain domain -> "`domain " ^ domain
	| `clear target -> "`clear " ^ (string_of_target target)
	| `cancel -> "`cancel"
	| `track_blur target -> "`track_blur " ^ (string_of_target target)
	| `track_focus target -> "`track_focus " ^ (string_of_target target)
	| `window_blur -> "`window_blur"
	| `window_focus -> "`window_focus"
	| `db_changed _ -> "`db_changed"
	| `accept_suggestion text -> "`accept_suggestion " ^ text
	| `select_cursor direction -> "`select_cursor " ^ (string_of_direction direction)
	| `select_suggestion idx -> "`select_suggestion " ^ (Option.to_string string_of_int idx)
	| `password_fully_selected sel -> "`password_fully_selected " ^ (string_of_bool sel)
	| `toggle_password_visibility -> "`toggle_password_visibility"
	| `generate_password -> "`generate_password"
	| `clipboard_supported supported -> "`clipboard_supported " ^ (string_of_bool supported)
	| `domain_form msg -> "`domain_form " ^ (Domain_form.string_of_message msg)

let add_canonical_tld_suggestion input suggestions =
	match Domain.guess input with
		| None -> suggestions
		| Some suggestion ->
			if List.mem suggestion suggestions
				then suggestions
				else suggestion :: suggestions

let update_suggestions state =
	let s = match state.active_input with
		| Some `domain -> if state.domain = "" then None else (
			Store.keys_like state.db state.domain
			|> add_canonical_tld_suggestion state.domain
			|> List.filter ((<>) state.domain)
			|> Option.non_empty ~zero:[]
		)
		| _ -> None
	in
	{ state with
		domain_suggestions = s |> Option.map (fun suggestions -> { suggestions; selected = None })
	}

let update : state -> message -> state =
	let update_domain_form state =
		{ state with domain_form =
			Domain_form.update state.domain_form (Domain_form.Domain_changed state.domain)
		}
	in
	let derive_domain_data state = state |> update_suggestions |> update_domain_form in
	let clear_generated_password state = { state with generated_password = None } in
	let modify_generated_password state fn =
		{ state with generated_password = state.generated_password |> Option.map fn }
	in
	let select_input target state = { state with active_input = Some target } in

	let rec update = (fun state -> function
		| `clear `password -> update state (`master_password "") |> select_input `password
		| `clear `domain -> update state (`domain "") |> select_input `domain
		| `cancel ->
			let update_with_focus target msg = update (update state (`track_focus target)) msg in
			if state.generated_password <> None then
				update_with_focus `password (`clear `password)
			else
				if state.master_password = "" && state.active_input = Some `password
					then update_with_focus `domain (`clear `domain)
					else update_with_focus `password (`master_password "")

		| `track_focus dest -> { state with active_input = Some dest }
		| `track_blur dest ->
				if state.active_input = Some dest
					then { state with active_input = None }
					else (
						Log.debug (fun m->m "received track_blur for %s, but active_input = %s"
							(string_of_target dest)
							(Option.to_string string_of_target state.active_input)
						);
						state
					)

		| `domain domain ->
			{ state with domain }
				|> derive_domain_data
				|> clear_generated_password

		| `master_password master_password ->
			{ state with master_password }
				|> clear_generated_password

		| `db_changed db -> { state with db } |> update_suggestions
		| `domain_form msg -> { state with domain_form = Domain_form.update state.domain_form msg }
		| `select_cursor direction ->
				let domain_suggestions = state.domain_suggestions |> Option.map (fun suggestions ->
					let idx = suggestions.selected |> Option.default (-1) in
					Log.debug (fun m->m"selecting %s from current idx %d" (string_of_direction direction) idx);
					let idx = match direction with
						| `down -> min ((List.length suggestions.suggestions) - 1) (idx + 1)
						| `up -> max (-1) (idx - 1)
					in
					{ suggestions with selected = idx |> Option.non_empty ~zero:(-1) }
				) in
				{ state with domain_suggestions }
		| `accept_suggestion domain ->
				{ state with
					domain;
					active_input = Some `password;
					domain_suggestions = None;
				} |> update_domain_form
		| `select_suggestion selected ->
			let domain_suggestions = state.domain_suggestions |> Option.map (fun suggestions ->
				{ suggestions with selected }
			) in
			{ state with domain_suggestions }
		| `password_fully_selected fully_selected ->
			modify_generated_password state (fun password ->
				{ password with fully_selected }
			)
		| `toggle_password_visibility ->
			modify_generated_password state (fun password ->
				{ password with visible = not password.visible }
			)
		| `generate_password ->
			{ state with active_input = None; generated_password = Some {
				password = Password.generate
					~domain:(Domain_form.current_of_state state.domain_form)
					state.master_password;
				visible = false;
				fully_selected = true;
			}}
		| `clipboard_supported clipboard_supported -> { state with clipboard_supported }

		(* used for command only: *)
		| `window_focus | `window_blur -> state
	) in
	update

let command ~sync ~emit =
	let blur_timeout = 5.0 in
	let timeout_generated_password = let open Lwt in Ui.supplantable (function
		| false -> return_unit
		| true ->
			let%lwt () = Lwt_js.sleep blur_timeout in
			Log.info (fun m->m "clearing generated password");
			(* cancels this branch because the other one is waiting
			 * on `password_input_data` changes *)
			emit `cancel;
			return_unit
	) in

	fun state msg -> match msg with
		| `domain_form msg ->
				Domain_form.command ~sync state.domain_form msg

		(* After generating a password, if the window stays blurred
		 * for more than a few seconds we clear the master password (to prevent
		 * leaving master passwords around)
		 *)
		| `window_blur when state.generated_password <> None ->
			Log.debug (fun m->m "window blurred; clearing pwd in %fs" blur_timeout);
			Some (timeout_generated_password true)
		| `window_focus | `master_password _ | `domain _ | `track_focus _ ->
			Log.debug (fun m->m "window back in focus or change made; clearing timeout");
			Some (timeout_generated_password false)

		| _ -> None

let id_counter = ref 0
let view instance : state -> message Html.html =
	let open Html in
	let open Bootstrap in
	let domain_form = Ui.child ~message:domain_form_message Domain_form.component instance in
	let instance_id = string_of_int !id_counter in
	let password_element_id = "password-contents-"^ instance_id in
	incr id_counter;
	let track_master_password = track_input_contents (fun text -> `master_password text) in
	let track_domain_text = track_input_contents (fun text -> `domain text) in
	let on_submit_password = handler (fun e ->
		e |> Event.keyboard_event |> Option.bind (fun e ->
			if Keycode.of_event e = Keycode.Enter
				then Some (Event.handle `generate_password)
				else None
		) |> Event.optional
	) in
	let apply_focus_state =
		let ensure_focus = (fun elem _attr ->
			Log.debug (fun m->m"focusing elment");
			Js.Opt.iter (Dom_html.CoerceTo.input elem) (fun elem -> elem##focus)
		) in
		let ensure_blur = (fun elem _attr ->
			Js.Opt.iter (Dom_html.CoerceTo.input elem) (fun elem ->
				if eqeqeq elem (Js.Unsafe.get Dom_html.document "activeElement") then (
					Log.debug (fun m->m"blurring elment");
					elem##blur
				)
			)
		) in
		fun field state ->
			let action = if state.active_input = (Some field) then ensure_focus else ensure_blur in
			a_dynamic "data-focus" action
	in
	let all_text_selected = a_dynamic "data-selected" (fun elem _attr ->
		let open Lwt in
		let (_:unit Lwt.t) =
			Lwt_js.yield () >>= fun () -> return (Dom_selection.select elem) in
		()
	) in
	let select_element = handler (fun event ->
		event |> Event.target |> Option.may Dom_selection.select;
		Event.handled
	) in
	let update_selection = handler @@ Ui.bind instance (fun { generated_password; _ } e ->
		generated_password |> Option.bind (fun { password; _ } ->
			e##.target |> Js.Opt.to_option |> Option.map (fun target ->
				let is_selected = Dom_selection.is_fully_selected ~length:(String.length password) target in
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
			if Keycode.of_event e = Keycode.Escape then (
				Some (Event.handle (`domain ""))
			) else (
				match Keycode.of_event e with
					| Keycode.ArrowUp -> Some (Event.handle (`select_cursor `up))
					| Keycode.ArrowDown -> Some (Event.handle (`select_cursor `down))
					| code -> (
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
						Optdef.iter e##.code (fun code ->
							Log.debug (fun m->m "Processing key code %s" (Js.to_string code))
						);
						begin match code with
							| Keycode.Tab -> select_current ()
							| Keycode.Enter -> select_current ()
							| _ -> Log.debug (fun m->m "ignoring unknown key code"); None
						end
				)
			)
		) |> Event.optional
	) in

	let query_selector sel =
		Dom_html.document##.documentElement##querySelector (Js.string sel) |> Js.Opt.to_option
	in

	let get_password_element () = query_selector ("#"^password_element_id) in

	let with_no_selected_inputs: 'a. (unit -> 'a) -> 'a = fun fn ->
		let initial = query_selector "input:focus" |> Option.map Dom_html.CoerceTo.input |> Option.bind Js.Opt.to_option in
		match initial with
			| None -> fn ()
			| Some initial ->
				let restore () = initial##focus in
				initial##blur;
				try (
					let result = fn () in
					restore ();
					result
				) with err -> (
					restore ();
					raise err
				)
	in

	let update_highlight () =
		get_password_element () |> Option.may (fun elem ->
			Ui.emit instance (`password_fully_selected (Dom_selection.is_fully_selected elem))
		)
	in

	let copy_generated_password = handler (fun _ ->
		get_password_element () |> Option.map (fun elem ->
			with_no_selected_inputs (fun () ->
				Dom_selection.select elem;
				Ui.emit instance (`password_fully_selected true);
				match Clipboard.triggerCopy () with
					| Some error ->
						Log.err (fun m->m "%s" error);
						Event.handle (`clipboard_supported false)
					| None ->
						(* give a bit of UI feedback in the default case that the
						 * selection has been copied *)
						Event.handle (`clear `password)
			)
		) |> Event.optional
	) in

	let root_hooks = with_global_listeners (fun () ->
		[
			global_event_listener Dom_html.Event.click (fun _ ->
				update_highlight ();
			);
			global_event_listener Dom_html.Event.keydown (fun e ->
				e |> Vdoml.Event.keyboard_event |> Option.may (fun e ->
					if Keycode.of_event e = Keycode.Escape
						then (
							Dom.preventDefault e;
							Ui.emit instance `cancel
						)
				)
			);
			global_event_listener ~target:Dom_html.window ~capture:false Dom_html.Event.blur (fun _ ->
				Ui.emit instance `window_blur
			);
			global_event_listener ~target:Dom_html.window ~capture:false Dom_html.Event.focus (fun _ ->
				Ui.emit instance `window_focus
			);
		] @ (Domain_form.global_listeners instance domain_form_message)
	) in

	(fun state ->
		let { domain; master_password; domain_suggestions; _ } = state in
		let no_input_coersion = [
			a_attr "autocomplete" "off";
			a_attr "autocorrect" "off";
			a_attr "autocapitalize" "off";
		] in

		let track_focus dest = [
			a_onblur @@ emitter (`track_blur dest);
			a_onfocus @@ emitter (`track_focus dest);
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
			apply_focus_state `domain state;
		] @ no_input_coersion @ track_focus `domain) () in

		let password_input = input ~a:([
			a_class "form-control password-input";
			a_input_type `Password;
			a_name "password";
			a_oninput track_master_password;
			a_onkeydown on_submit_password;
			a_value master_password;
			apply_focus_state `password state;
		] @ no_input_coersion @ track_focus `password) () in

		let clear_btn ?right target : message html =

			span ~a:[
				a_class "link text-muted clear-btn";
				a_style ("right:"^(right |> Option.default 25 |> string_of_int)^"px;");
				a_onclick @@ emitter (`clear target);
			] [icon "remove"]
		in

		let make_suggestion_ui : domain_suggestions option -> message html = function
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
					a_onclick select_element;
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

				col [domain_form state.domain_form];
			];
		] in

		form |> root_hooks
	)

let component = Ui.component ~eq ~view ()
