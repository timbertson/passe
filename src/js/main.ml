open Passe
open Lwt
open Js
open Dom_html
open Common
open React_ext
module J = Json_ext

let s = Js.string
let log = Logging.get_logger "main"
let non_empty_string : string -> string option = Option.non_empty ~zero:""
let default_empty_string : string option -> string = Option.default ""

exception Fail

let check cond = Printf.ksprintf (function s ->
	if cond then () else raise (AssertionError s)
	)

let is_within min max i = i >= min && i <= max
let within min max i = Pervasives.min (Pervasives.max i min) max

let incognito, set_incognito = S.create false

let db_display sync : #Dom_html.element Ui.widget =
	let contents:string signal = sync.Sync.stored_json |> S.map (fun json ->
		json
			|> Option.map (J.to_string)
			|> Option.default "<no DB>"
	) in

	let display = Ui.div ~cls:"db"
		~children:[
			Ui.text_stream contents
		] () in

	Ui.div ~cls:"db-editor" ~children:[
		Ui.frag display;
	] ()

let footer () =
	let open Ui in

	let incognito_checkbox = Ui.checkbox_of_signal ~update:set_incognito incognito in
	incognito_checkbox#attr "title" "Don't store anything on this browser";

	let incognito_container = div ~cls:"incognito-checkbox" ~children:[
		frag incognito_checkbox;
		child span ~text:" Incognito mode" ();
	] () in

	incognito_container#class_s "selected" incognito;

	row `XS [
		col [
			child ul ~cls:"list-unstyled" ~children:[
				child li ~children:[
					frag incognito_container;
				] ();
			] ();
		];
		col ~cls:"text-right" [
			child ul ~cls:"list-unstyled" ~children:[
				child li ~cls:"link" ~text:"About this site" ~mechanism:(fun elem ->
					Lwt_js_events.clicks elem (fun e _ ->
						Ui.stop e;
						Ui.overlay (fun close ->
							Ui.panel ~close ~title:"About Passé" ~children:[
								child div ~mechanism:(fun elem ->
									elem##innerHTML <- Js.string (
										About.aboutHtml ^ "\n<hr/><small>Version " ^ (Version.pretty ()) ^ "</small>";
									);
									return_unit
								) ();
							] ()
						)
					)
				) ();
			] ()
		];
	]

let password_form sync : #Dom_html.element Ui.widget =
	let db_fallback = sync.Sync.db_fallback in
	let open Ui in

	let domain, set_domain = S.create "" in

	let domain_is_active, set_domain_is_active = S.create false in
	let domain_input = Ui.input_of_signal ~update:set_domain domain in
	domain_input#attr "name" "domain";

	let master_password, set_master_password = S.create "" in
	let password_input :Dom_html.inputElement widget = Ui.input_of_signal
		~cons:(fun () -> createInput document ~_type:(s"password") ~name:(s"password"))
		~update:set_master_password
		master_password
	in
	domain_input#attr "class" "form-control";
	password_input#attr "class" "form-control password";

	let empty_domain = domain |> S.map (fun d -> d = "") in
	let domain_record = S.l2 (fun db dom -> Store.lookup dom db) db_fallback domain in
	let saved_domain_info = S.l3 (fun db domain text ->
		match domain with
			| Some d -> d
			| None -> Store.default db text
	) db_fallback domain_record domain in
	let domain_is_unknown = (S.map Option.is_none domain_record) in

	let domain_info, update_domain_info = editable_signal saved_domain_info in
	let update_domain_info = fun v ->
		log#info "updating domain info to %s" (Store.json_string_of_domain v); update_domain_info v in


	(* build up the generated password.
	 * It tracks (master_password, domain_info), but only
	 * sampled on form_submits. Whenever the inputs change or
	 * clear_generated_password is triggered, the inputs are
	 * overridden to be `None`, so the password is removed.
	 *)
	let password_input_data = S.l2 (fun a b -> (a,b)) master_password domain_info in
	let password_resets, clear_generated_password = E.create () in
	let password_resets =
		(* reset password info to `None` when a reset is explicitly triggered,
		 * or when the input data changes *)
		let to_none = fun _ -> None in
		E.select [
			password_resets |> E.map to_none;
			S.changes password_input_data |> E.map to_none;
		] in

	let form_submits, submit_form = E.create () in
	let password_submissions = S.sample (fun () v -> v) form_submits password_input_data
		|> E.map (fun inputs -> (Some inputs)) in

	let current_password = E.select [password_resets; password_submissions]
		|> S.hold None
		|> S.map (fun inputs -> inputs
			|> Option.map (fun (password, domain)
				-> Password.generate ~domain password
			)
		)
	in

	let to_html_elem : Dom.node Js.t -> Dom_html.element Js.t Js.opt = fun node ->
		let elem = Dom.CoerceTo.element node in
		Opt.map elem Dom_html.element
	in

	let upto_class cls (elem:#Dom_html.element Js.t) =
		let cls = Js.string cls in
		let rec up (elem:Dom_html.element Js.t) =
			if (elem##classList##contains(cls) |> Js.to_bool)
				then Opt.return elem
				else (
					let elem = Opt.bind elem##parentNode to_html_elem in
					Opt.bind elem up
				)
		in
		up elem
	in

	let clear_btn ?right ?trigger () =
		child span ~cls:"link text-muted clear-btn"
			~attrs:["style","right:"^(right |> Option.default 25 |> string_of_int)^"px;"]
			~children:[icon "remove"]
			~mechanism:(fun elem ->
				let container = elem |> upto_class "form-group" in
				Lwt_js_events.clicks elem (fun event _ ->
					let input = Opt.bind container (fun el -> el##querySelector (Js.string"input")) in
					let input = Opt.bind input Dom_html.CoerceTo.input in
					let input = Opt.get input (fun () -> failwith "can't find input for clear_button") in

					clear_generated_password ();
					trigger |> Option.may (fun f -> f ());
					input##value <- Js.string"";
					input##focus ();
					return_unit
				)
			) ();
	in

	let disable_autocomplete input = input#attr "autocomplete" "off" in
	disable_autocomplete domain_input;
	disable_autocomplete password_input;

	let _domain_suggestions = S.l2 (fun db query ->
		if query = "" then None else (
			Store.keys_like db query
			|> List.filter ((<>) query)
			|> Option.non_empty ~zero:[]
		)
	) db_fallback domain in
	let domain_suggestions = S.l2 (fun active suggestions ->
		if active
		then suggestions
		else None
	) domain_is_active _domain_suggestions in
	let suggestion_idx, set_suggestion_idx = S.create None in
	let select_diff d =
		let max_len:int = match S.value _domain_suggestions with
			| None -> 0
			| Some l -> (List.length l) - 1
		in
		let desired:int = match S.value suggestion_idx with
			| None -> if d > 0 then 0 else -1
			| Some i -> (max (-1) i) + d
		in
		set_suggestion_idx (Some (min desired max_len));
	in
	
	let keycode_return = 13 in
	let keycode_tab = 9 in
	let accept_suggestion : 'a. string -> (#Dom_html.element as 'a) Js.t -> unit
		= fun text elem ->
		set_domain text;
		let container = (elem:>Dom_html.element Js.t) |> upto_class "password-form" in
		let input = Opt.bind container (fun el -> el##querySelector (Js.string"input[type=\"password\"]")) in
		let input = Opt.bind input Dom_html.CoerceTo.input in
		match input |> Opt.to_option with
			| Some input -> input##focus()
			| None -> log#error "can't find input for clear_button"
	in

	domain_input#class_s "suggestions" (S.map Option.is_some domain_suggestions);
	domain_input#mechanism (fun elem ->
		elem##focus();
		set_domain_is_active true;
		Lwt.join [
			Lwt_js_events.focuses  elem (fun _ _ -> set_domain_is_active true; return_unit);
			Lwt_js_events.blurs    elem (fun _ _ -> set_domain_is_active false; return_unit);
			Lwt_js_events.inputs   elem (fun _ _ -> set_suggestion_idx None; return_unit);
			Lwt_js_events.keydowns elem (fun e _ ->
				e##keyIdentifier |> Optdef.to_option
				|> Option.or_fn (fun () -> Unsafe.get e "key" |> Optdef.to_option)
				|> Option.may (fun ident ->
					match Js.to_string ident with
					| "Up" | "ArrowUp" -> select_diff (-1); stop e
					| "Down" | "ArrowDown" -> select_diff 1; stop e
					| _ -> ()
				);
				let which = Unsafe.get e "which" in
				let select_current () =
					let selected = ref false in
					S.value suggestion_idx |> Option.may (fun idx ->
						S.value _domain_suggestions |> Option.may (fun l ->
							try (
								accept_suggestion (List.nth l idx) elem;
								selected := true
							) with Not_found -> ()
						)
					);
					!selected
				in

				begin match which with
					| k when k = keycode_tab -> if (select_current ()) then stop e
					| k when k = keycode_return -> if (select_current ()) then stop e
					| k -> log#debug "ignoring unknown key code %d" k
				end;
				Console.console##log(e);
				return_unit
			);
		]
	);

	let make_suggestion_ui (suggestions, idx) =
		let idx = Option.default (-1) idx in
		let parent_mech = (fun elem ->
			Lwt_js_events.mouseouts elem (fun _ _ ->
				set_suggestion_idx None;
				return_unit)
		) in

		ul ~cls:"suggestions" ~children: (
			suggestions |> List.mapi (fun i text ->
				let w = li ~text ~mechanism:(fun elem ->
					Lwt_js_events.mousedowns elem (fun click _ ->
						Ui.stop click;
						accept_suggestion text elem;
						return_unit
					) <&>
					Lwt_js_events.mouseovers elem (fun e _ ->
						set_suggestion_idx (Some i);
						return_unit
					)
				) () in
				if i = idx then w#attr "class" "selected";
				frag w
			)
		) ~mechanism:parent_mech ()
	in

	let open Store in
	let domain_info_editor ~get ~set = input_of_signal
		~update:(fun v -> update_domain_info (set v))
		(saved_domain_info |> S.map get)
	in
	
	let notes_input = domain_info_editor
		~get:(fun d -> d.note |> default_empty_string)
		~set:(fun v -> {S.value saved_domain_info with note=non_empty_string v}) in

	let length_input = domain_info_editor
		~get:(fun d -> d.length |> string_of_int)
		~set:(fun v -> {S.value saved_domain_info with length=int_of_string v}) in

	let suffix_input = domain_info_editor
		~get:(fun d -> d.suffix |> default_empty_string)
		~set:(fun v -> {S.value saved_domain_info with suffix=non_empty_string v}) in

	let show_plaintext_password, set_show_plaintext_password = S.create false in
	let plaintext_toggle_mech = (fun elem ->
		Lwt_js_events.clicks ~use_capture:true elem (fun e _ ->
			Ui.stop e;
			set_show_plaintext_password (not @@ S.value show_plaintext_password);
			return_unit
		)
	) in
	let clear_generated_password_mech = (fun elem ->
		Lwt_js_events.clicks elem (fun e _ ->
			Ui.stop e;
			clear_generated_password ();
			return_unit
		)
	) in

	let password_display = current_password |> Ui.optional_signal_content (fun (p:string) ->
		let length = String.length p in
		let is_selected, set_is_selected = S.create true in

		let string_repeat s n = Array.fold_left (^) "" (Array.make n s) in
		let dummy_text = (string_repeat "•" length) in
		let dummy = span ~cls:"dummy"
			~children: [
				frag (Ui.text_stream (show_plaintext_password |>
				S.map (fun plain -> if plain then p else dummy_text)))
			]
			()
		in
		dummy#class_s "selected" is_selected;

		let display = div
			~cls:"password-display"
			~mechanism:(fun elem ->
				let child = elem##querySelector(Js.string ".secret") |> non_null in
				let select () =
					Selection.select child;
					set_is_selected true
				in
				let update_highlight () =
					set_is_selected @@ Selection.is_fully_selected ~length child;
				in

				select ();

				effectful_stream_mechanism (show_plaintext_password |> S.map (fun _ -> select ())) <&>
				Lwt_js_events.clicks ~use_capture:true document (fun e _ ->
					update_highlight ();
					Lwt.return_unit
				) <&>
				Lwt_js_events.keyups ~use_capture:true document (fun e _ ->
					if e##shiftKey == Js._true then
						update_highlight ();
					Lwt.return_unit
				) <&>
				Lwt_js_events.clicks ~use_capture:false elem (fun e _ ->
					select ();
					Lwt.return_unit
				)
			) ~children:[
				child span ~cls:"secret" ~text:p ();
				frag dummy;
			] () in

		div ~cls:"popover static password-popover fade bottom in" ~attrs:["role","tooltip"] ~children:[
			child div ~cls:"arrow" ();
			child div ~cls:"popover-content" ~children:[
				child table ~children:[
					child tr ~children:[
						child td ~children:[
							child h4 ~cls:"title visible-lg visible-md" ~text:"Generated: " ();
						] ();

						child td ~attrs:["width","*"] ~children:[
							frag display;
						] ();

						child td ~cls:"controls" ~children:[
							child span ~cls:"toggle"
								~mechanism:plaintext_toggle_mech
								~children:[icon "eye-open"] ();
						] ();
						child td ~cls:"controls" ~children:[
							child span ~cls:"toggle"
								~mechanism:clear_generated_password_mech
								~children:[icon "remove"] ();
						] ();
					] ();
				] ();
			] ();
		] ();
	) |> Ui.stream in

	let unchanged_domain = S.l2 (fun db_dom domain_info ->
		match db_dom with
			| Some db -> Store.record_eq (Domain db) (Domain domain_info)
			| None -> false
	) domain_record domain_info in

	let save_current_domain () =
		let (_saved:bool) = Sync.save_change ~state:sync
			~original:(S.value domain_record |> Option.map (fun d -> Domain d))
			(Some (Domain (S.value domain_info))) in
		()
	in

	let domain_panel = Ui.div ~cls:"domain-info panel" () in
	let () =
		let open Store in
		let open Ui in
		domain_panel#class_s "unknown" domain_is_unknown;
		domain_panel#class_s "hidden" empty_domain;

		let no_user = sync.Sync.current_username |> S.map Option.is_none in
		let save_button = input ~attrs:[("type","button");("value","save");("title","(ctrl+s)")] () in
		save_button#class_s "hidden" (S.l2 (||) no_user unchanged_domain);

		save_button#mechanism (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				Ui.stop event;
				save_current_domain ();
				return_unit
			)
		);

		let delete_button = a ~cls:"delete link" ~children:[icon "remove"] () in
		delete_button#class_s "hidden" (S.l2 (||) no_user domain_is_unknown);
		delete_button#mechanism (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				stop event;
				let (_saved:bool) = Sync.save_change ~state:sync
					~original:(S.value domain_record |> Option.map (fun d -> Domain d))
					None in
				return_unit
			)
		);

		let inline_input text input =
			child div ~cls:"inline" ~children:[
				child strong ~text: text ();
				child span ~children:[
					frag input;
				] ();
			] ()
		in


		domain_panel#append_all [
			child div ~cls:"panel-heading" ~children:[
				child h3 ~children: [
					frag delete_button;
					(S.l3 (fun domain unknown unchanged ->
						if unknown || unchanged
							then domain
							else domain^" *")
						domain domain_is_unknown unchanged_domain
					) |> Ui.text_stream;

					frag save_button;
				] ();
			] ();
			child div ~cls:"panel-body" ~children: [
				inline_input "Length:" length_input;
				inline_input "Notes:" notes_input;
				inline_input "Suffix:" suffix_input;
			] ();
		];
	in

	let form = Ui.form ~cls:"form-horizontal main-form" ~attrs:(["role","form"]) ~children:[
		let left elem = col ~size:2 [elem] in

		row `Sml [
			col ~size:7 ~cls:"password-form" [
				row `XS ~collapse:true ~cls:"form-group" [
					left @@ control_label "Domain";
					col [
						clear_btn ~trigger:(fun () -> set_domain "") ();
						frag domain_input;
						Ui.stream (
							S.l2 (fun l idx -> l |> Option.map (fun l -> (l, idx))) domain_suggestions suggestion_idx
							|> optional_signal_content make_suggestion_ui
						);
					];
				];

				row `XS ~collapse:true ~cls:"form-group" [
					left @@ control_label "Password";
					col [
						clear_btn ~right:68 ();
						child div ~cls:"input-group" ~children:[
							frag password_input;
							child span ~cls:"input-group-btn" ~children:[
								child button ~cls:"btn btn-default" ~attrs:[("type", "submit")] ~children:[icon "play"] ();
							] ();
						]();
						frag password_display;
					];
				];
			];

			col [frag domain_panel];
		];
	] () in

	let keycode_s = 83 in
	form#mechanism (fun elem ->
		Lwt_js_events.keydowns ~use_capture:true elem (fun event _ ->
			Console.console##log(event);
			if (to_bool event##ctrlKey && event##keyCode = keycode_s) then (
				stop event;
				save_current_domain ();
				return_unit
			) else return_unit
		)
		<&>
		Lwt_js_events.submits elem (fun event _ ->
			Ui.stop event;
			log#info "form submitted";
			elem##querySelectorAll(s"input") |> Dom.list_of_nodeList
				|> List.iter (fun elem ->
						let input = CoerceTo.input(elem) |> non_null in
						input##blur()
				);
			submit_form ();
			Lwt.return_unit
		)
	);
	form

let show_form sync (container:Dom_html.element Js.t) =
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	let all_content = Ui.div
		~children:[
			Ui.child Ui.div ~cls:"container" ~children:[
				Ui.frag @@ Sync_ui.ui sync;
				Ui.frag @@ password_form sync;
			] ();
			Ui.child Ui.div ~cls:"container footer" ~children:[
				Ui.frag @@ footer ();
			]()
		] () in
	(* all_content#append @@ db_display (); *)
	Ui.withContent container all_content (fun _ ->
		lwt () = Ui.pause () in
		Lwt.return_unit
	)

let print_exc context e =
	log#error "Uncaught %s Error: %s\n%s"
		context
		(Printexc.to_string e)
		(Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string)

let () = Lwt.async_exception_hook := print_exc "Uncaught LWT"

let main sync = Lwt.async (fun () ->
	try_lwt (
		let main_elem = (document##getElementById (s"main")) in
		check (Opt.test main_elem) "main_elem not found!";
		let main_elem = Opt.get main_elem (fun _ -> raise Fail) in
		lwt () =
			show_form sync main_elem
			<&>
			App_cache.update_monitor (fun () ->
				log#info("appcache update ready");
				let busy = document##body##querySelector(Js.string"input:focus")
					|> Opt.to_option
					|> Option.map (fun elem ->
							let value = (Js.Unsafe.get elem (Js.string"value")) in
							value##length > 0
					) |> Option.default false
				in
				begin if busy then
					log#warn "Not reloading; active input is nonempty"
				else
					Dom_html.window##location##reload()
				end;
				return_unit)
			<&>
			App_cache.poll_server ()
		in
		return_unit
	) with e -> (
		print_exc "Toplevel" e;
		return_unit
	)
)

let () =
	log#info "passe %s" (Version.pretty ());
	Logging.(current_level := ord (
		let uri = !Server.root_url in
		match Uri.fragment uri with
		| Some "debug" -> Debug
		| Some "info" -> Info
		| _ -> (match Uri.host uri with
			| Some "localhost" -> Info
			| _ -> Warn
		)
	));

	let storage_provider = (new Local_storage.provider (true)) in
	let config_provider = Config.build storage_provider in
	let _ = incognito |> S.map (fun v -> storage_provider#set_persistent (not v)) in
	let sync = Sync.build config_provider in

	let listener = ref null in
	listener := Opt.return @@ Dom_events.listen
		window
		(Event.make "DOMContentLoaded")
		(fun _ _ ->
			Opt.iter !listener Dom_events.stop_listen;
			main sync;
			false
		)

