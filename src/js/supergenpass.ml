open Lwt
open Js
open Dom_html
open Common
open React_ext
module J = Yojson.Safe

let s = Js.string
let log = Logging.get_logger "main"
let non_empty_string : string -> string option = Option.non_empty ~zero:""
let default_empty_string : string option -> string = Option.default ""

exception Fail

let check cond = Printf.ksprintf (function s ->
	if cond then () else raise (AssertionError s)
	)

let local_db = new Local_storage.record "db"
let db_signal =
	let initial = local_db#get
		|> Option.map Store.parse_json |> Option.default Store.empty in
	let signal, update = S.create initial in
	local_db#watch (fun db -> update (Store.parse_json db));
	signal

let is_within min max i = i >= min && i <= max
let within min max i = Pervasives.min (Pervasives.max i min) max

let db_editor () : #Dom_html.element Ui.widget =
	let contents, update_contents = editable_signal (db_signal |> S.map (fun db ->
		db |> Store.to_json_string
	)) in

	let error_text, set_error_text = S.create None in
	let textarea = Ui.textarea_of_signal
		~events:Lwt_js_events.changes
		~update:(fun contents ->
			log#info "got input text: %s" contents;
			match Store.parse contents with
				| Left err ->
					set_error_text (Some err)
				| Right db ->
					set_error_text None;
					log#info "got db: %s" (Store.to_json_string db);
					local_db#save (Store.to_json db)
		) contents in

	textarea#attr "rows" "10";
	textarea#attr "cols" "60";
	let error_dom_stream = error_text |> Ui.optional_signal_content (fun err ->
		Ui.div ~cls:"error" ~text:err ()
	) in
	let error_elem = Ui.stream error_dom_stream in
	Ui.div ~cls:"db-editor" ~children:[
		Ui.frag error_elem;
		Ui.frag textarea;
	] ()

let password_form () : #Dom_html.element Ui.widget =
	let open Ui in

	let current_password, set_current_password = S.create None in
	let invalidate_password = fun elem ->
		Lwt_js_events.inputs elem (fun event _ ->
			set_current_password None;
			Lwt.return_unit
		) in
	let domain, set_domain = S.create "" in

	let domain_is_active, set_domain_is_active = S.create false in
	let domain_input = Ui.input_of_signal ~update:set_domain domain in
	domain_input#attr "name" "domain";

	let password_input = element (fun () -> createInput document ~_type:(s"password") ~name:(s"password")) in
	domain_input#attr "class" "form-control";
	password_input#attr "class" "form-control";
	password_input#mechanism invalidate_password;

	let master_password = Ui.signal_of_input ~events:Lwt_js_events.inputs password_input in
	let empty_domain = domain |> S.map (fun d -> d = "") in
	let domain_record = S.l2 (fun db dom -> Store.lookup dom db) db_signal domain in
	let saved_domain_info = S.l2 (fun domain text ->
		match domain with
			| Some d -> d
			| None -> Store.default text
	) domain_record domain in
	let domain_is_unknown = (S.map Option.is_none domain_record) in

	let _domain_suggestions = S.l2 (fun db query ->
		if query = "" then None else (
			Store.keys_like db query
			|> List.filter ((<>) query)
			|> Option.non_empty ~zero:[]
		)
	) db_signal domain in
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
		log#info "new: %d" (S.value suggestion_idx |> Option.default 9999);
	in
	
	domain_input#class_s "suggestions" (S.map Option.is_some domain_suggestions);
	domain_input#mechanism (fun elem -> elem##focus();
		set_domain_is_active true;
		Lwt.join [
			invalidate_password elem;
			Lwt_js_events.focuses elem (fun _ _ -> set_domain_is_active true; return_unit);
			Lwt_js_events.blurs   elem (fun _ _ -> set_domain_is_active false; return_unit);
			Lwt_js_events.inputs   elem (fun _ _ -> set_suggestion_idx None; return_unit);
			Lwt_js_events.keydowns elem (fun e _ ->
				log#info "keydowns!";
				Optdef.iter (e##keyIdentifier) (fun ident ->
					match Js.to_string ident with
					| "Up" -> select_diff (-1); stop e
					| "Down" -> select_diff 1; stop e
					| _ -> ()
				);
				let which = Unsafe.get e "which" in
				let select_current () =
					let selected = ref false in
					S.value suggestion_idx |> Option.may (fun idx ->
						log#info "%d" idx;
						S.value _domain_suggestions |> Option.may (fun l ->
							try (
								set_domain @@ (List.nth l idx);
								selected := true
							) with Not_found -> ()
						)
					);
					!selected
				in

				begin match which with
					| 9 -> ignore (select_current ())
					| 13 -> if (select_current ()) then stop e;
					| _ -> ()
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
						set_domain text;
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

	let domain_info, update_domain_info = editable_signal saved_domain_info in
	let update_domain_info = fun v ->
		log#info "updating domain info to %s" (Store.json_string_of_domain v); update_domain_info v in

	let open Store in
	let domain_info_editor ~get ~set = input_of_signal
		~update:(fun v -> update_domain_info (set v))
		(saved_domain_info |> S.map get)
	in
	
	let hint_input = domain_info_editor
		~get:(fun d -> d.hint |> default_empty_string)
		~set:(fun v -> {S.value saved_domain_info with hint=non_empty_string v}) in

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

	let password_display = current_password |> Ui.optional_signal_content (fun (p:string) ->
		let length = String.length p in
		let is_selected, set_is_selected = S.create true in
		let dummy_text = (String.make length '*') in
		let dummy = span ~cls:"dummy"
			~children: [
				frag (Ui.text_stream (show_plaintext_password |>
				S.map (fun plain -> if plain then p else dummy_text)))
			]
			()
		in
		dummy#class_s "selected" is_selected;

		div
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

				Lwt_js_events.clicks ~use_capture:true document (fun e _ ->
					update_highlight ();
					Lwt.return_unit
				) <&>
				Lwt_js_events.keyups ~use_capture:true document (fun e _ ->
					if e##shiftKey == Js._true then
						update_highlight ();
					Lwt.return_unit
				) <&>
				Lwt_js_events.clicks ~use_capture:true elem (fun e _ ->
					select ();
					Lwt.return_unit
				)
			) ~children:[
				child span ~cls:"secret" ~text:p ();
				frag dummy;
				child span ~cls:"toggle" ~mechanism:plaintext_toggle_mech ~children:[
					child span ~cls:"glyphicon glyphicon-eye-open" ();
				] ();
			] ()
	) |> Ui.stream in

	let unchanged_domain = S.l2 (fun db_dom domain_info ->
		match db_dom with
			| Some db -> Store.eq (Domain db) (Domain domain_info)
			| None -> false
	) domain_record domain_info in

	let domain_panel = Ui.div ~cls:"domain-info panel" () in
	let () =
		let open Store in
		let open Ui in
		domain_panel#class_s "unknown" domain_is_unknown;
		domain_panel#class_s "hidden" empty_domain;

		let save_button = input ~attrs:[("type","button");("value","save")] () in
		save_button#class_s "hidden" unchanged_domain;
		save_button#mechanism (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				Ui.stop event;
				let current_db = S.value db_signal in
				let new_db = Store.update
					~db:current_db
					~original:(S.value domain_record |> Option.map (fun d -> Domain d))
					(Domain (S.value domain_info)) in

				log#info "Saving new DB: %s" (Store.to_json_string new_db);
				local_db#save (Store.to_json new_db);
				Lwt.return_unit
			)
		);

		domain_panel#append_all [
			child div ~cls:"panel-heading" ~children:[
				child h3 ~children: [
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
				child div ~cls:"inline" ~children:[
					child strong ~text: "Length: " ();
					child span ~children:[
						frag length_input;
					] ();
				] ();

				child div ~cls:"inline" ~children:[
					child strong ~text: "Hint: " ();
					child span ~children:[
						frag hint_input;
					] ();
				] ();

				child div ~cls:"inline" ~children:[
					child strong ~text: "Suffix: " ();
					child span ~children:[
						frag suffix_input;
					] ();
				] ();

			] ();
		];
	in



	let form = Ui.form ~cls:"form-horizontal" ~attrs:(["role","form"]) ~children:[
		child div ~cls:"row" ~children:[
			child div ~cls:"col-sm-7" ~children:[
				child div ~cls:"col-xs-offset-2" ~children:[
					child h1 ~text:"SuperGenPass" ();
				] ();
			] ();
		] ();

		child div ~cls:"row" ~children:[
			child div ~cls:"col-sm-7" ~children:[
				child div ~cls:"form-group" ~children:[
					child label ~cls:"col-xs-2 control-label" ~text:"Domain" ();
					child div ~cls:"col-xs-10" ~children:[
						frag domain_input;
						Ui.stream (
							S.l2 (fun l idx -> l |> Option.map (fun l -> (l, idx))) domain_suggestions suggestion_idx
							|> optional_signal_content make_suggestion_ui
						);
					] ();
				] ();
				child div ~cls:"form-group" ~children:[
					child label ~cls:"col-xs-2 control-label" ~text:"Password" ();
					child div ~cls:"col-xs-10" ~children:[
						frag password_input;
					] ();
				] ();

				child div ~cls:"form-group" ~children:[
					child div ~cls:"col-xs-offset-2 col-xs-2" ~children:[
						child input ~cls:"btn btn-primary" ~attrs:[("type", "submit");("value","Generate")] ();
					] ();
					child div ~cls:"col-xs-8" ~children:[
						frag password_display;
					] ();
				] ();
			] ();

			child div ~cls:"col-sm-5" ~children:[
				frag domain_panel;
			] ();
		] ();
	] () in


	form#mechanism (fun elem ->
		Lwt_js_events.submits elem (fun event _ ->
			Ui.stop event;
			log#info "form submitted";
			elem##querySelectorAll(s"input") |> Dom.list_of_nodeList
				|> List.iter (fun elem ->
						let input = CoerceTo.input(elem) |> non_null in
						input##blur()
				);

			(* TODO: store & retrieve defaults in DB *)
			let domain = S.value domain_info in
			let password = S.value master_password in
			let password = Password.generate ~domain password in
			log#warn "generated: %s" password;
			set_current_password (Some password);
			(* TODO: highlight generated password *)
			Lwt.return_unit
		)
	);
	form

let show_form (container:Dom_html.element Js.t) =
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	log#info "Hello container!";
	let all_content = Ui.div () in
	all_content#append @@ password_form ();
	all_content#append @@ db_editor ();
	all_content#append @@ Sync.ui ();
	Ui.withContent container all_content (fun _ ->
		lwt () = Ui.pause () in
		log#info "ALL DONE";
		Lwt.return_unit
	)

let print_exc context e =
	log#error "Uncaught %s Error: %s\n%s"
		context
		(Printexc.to_string e)
		(Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string)

let () = Lwt.async_exception_hook := print_exc "Uncaught LWT"

let main () = Lwt.async (fun () ->
	try_lwt (
		let main_elem = (document##getElementById (s"main")) in
		check (Opt.test main_elem) "main_elem not found!";
		let main_elem = Opt.get main_elem (fun _ -> raise Fail) in
		lwt () = show_form main_elem in
		return_unit
	) with e -> (
		print_exc "Toplevel" e;
		return_unit
	)
)

let () =
	log#info "main";
	let listener = ref null in
	listener := Opt.return @@ Dom_events.listen
		window
		(Event.make "DOMContentLoaded")
		(fun _ _ ->
			Opt.iter !listener Dom_events.stop_listen;
			log#info "main";
			main ();
			false
		)

