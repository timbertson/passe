open Lwt
open Js
open Dom_html
open Common
open Lwt_react
module J = Yojson.Safe

let s = Js.string
let log = Logging.get_logger "main"
let non_empty_string : string -> string option = Option.non_empty ~zero:""
let default_empty_string : string option -> string = Option.default ""

exception Fail

let check cond = Printf.ksprintf (function s ->
	if cond then () else raise (AssertionError s)
	)

let editable_signal source =
	let derived, update = S.create (S.value source) in
	let effect = source |> S.map update in
	ignore @@ S.retain derived (fun () -> ignore effect; ());
	(derived, update)

let local_db = new Local_storage.record "db"
let db_signal =
	let initial = local_db#get
		|> Option.map Store.parse_json |> Option.default Store.empty in
	let signal, update = S.create initial in
	local_db#watch (fun db -> update (Store.parse_json db));
	signal

let optional_signal_content : ('a -> #Dom.node Ui.widget_t) -> 'a option React.signal -> Dom.node Ui.widget_t signal =
	fun f signal ->
		let empty = Ui.none document in
		signal |> S.map (fun value -> match value with
			| Some value -> ((f value):>#Dom.node Ui.widget_t)
			| None -> (empty:>Dom.node Ui.widget_t)
		)


let db_editor () : #Dom_html.element Ui.widget =
	let textarea = Ui.textArea () in
	let error_text, set_error_text = S.create None in

	textarea#attr "rows" "10";
	textarea#attr "cols" "60";
	textarea#mechanism (fun elem ->
		log#info "Mechanism is running!";
		local_db#get_str |> Option.may (fun s -> elem##value <- s);
		Lwt_js_events.buffered_loop Lwt_js_events.input elem (fun evt _ ->
			let contents = elem##value |> Js.to_string in
			log#info "got input text: %s" contents;
			begin match Store.parse contents with
				| Left err ->
						set_error_text (Some err)
				| Right db ->
						set_error_text None;
						log#info "got db: %s" (Store.to_json_string db);
						local_db#save (Store.to_json db)
			end;
			Lwt.return_unit
		)
	);
	let error_dom_stream = error_text |> optional_signal_content (fun err ->
		let div = Ui.div () in
		div#attr "class" "error";
		div#append @@ Ui.text err;
		(div:>Dom.node Ui.widget_t) (* XXX remove cast *)
	) in
	let error_elem = Ui.stream error_dom_stream in
	let result = Ui.div ~cls:"db-editor" () in
	result#append error_elem;
	result#append textarea;
	result

let password_form () : #Dom_html.element Ui.widget =
	let open Ui in

	let current_password, set_current_password = S.create None in
	let invalidate_password = fun elem ->
		Lwt_js_events.inputs elem (fun event _ ->
			set_current_password None;
			Lwt.return_unit
		) in

	let domain_input =   element (fun () -> createInput document ~_type:(s"text")     ~name:(s"domain")) in
	let password_input = element (fun () -> createInput document ~_type:(s"password") ~name:(s"password")) in
	domain_input#attr "class" "form-control";
	domain_input#mechanism (fun elem -> elem##focus(); invalidate_password elem);
	password_input#attr "class" "form-control";
	password_input#mechanism invalidate_password;

	let domain = Ui.signal_of_input ~events:Lwt_js_events.inputs domain_input in
	let master_password = Ui.signal_of_input ~events:Lwt_js_events.inputs password_input in
	let empty_domain = domain |> S.map (fun d -> d = "") in
	let domain_record = S.l2 (fun db dom -> Store.lookup dom db) db_signal domain in
	let saved_domain_info = S.l2 (fun domain text ->
		match domain with
			| Some d -> d
			| None -> Store.default text
	) domain_record domain in
	let domain_is_unknown = (S.map Option.is_none domain_record) in

	let domain_info, update_domain_info = editable_signal saved_domain_info in
	let update_domain_info = fun v ->
		log#info "updating domain info to %s" (Store.json_string_of_domain v); update_domain_info v in

	let open Store in
	let hint_input = input_of_signal
		~update:(fun v -> update_domain_info ({S.value saved_domain_info with hint=non_empty_string v}))
		(saved_domain_info |> S.map (fun d -> d.hint |> default_empty_string)) in

	let length_input = input_of_signal
		~update:(fun v -> update_domain_info ({S.value saved_domain_info with length=int_of_string v}))
		(saved_domain_info |> S.map (fun d -> d.length |> string_of_int)) in

	let suffix_input = input_of_signal
		~update:(fun v -> update_domain_info ({S.value saved_domain_info with suffix=non_empty_string v}))
		(saved_domain_info |> S.map (fun d -> d.suffix |> default_empty_string)) in

	let password_display = current_password |> optional_signal_content (fun p ->
		let length = String.length p in
		let is_selected, set_is_selected = S.create true in
		let dummy = span ~cls:"dummy"
			~text:(String.make length '*') ()
		in
		dummy#class_s "selected" is_selected;

		let container = div
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
			)
			~children:[
				child span ~cls:"secret" ~text:p ();
				frag dummy;
			]() in

		(container:>Dom.node Ui.widget_t)
	) |> Ui.stream in

	let domain_panel = Ui.div ~cls:"domain-info panel" () in
	let () =
		let open Store in
		let open Ui in
		domain_panel#class_s "unknown" domain_is_unknown;
		domain_panel#class_s "hidden" empty_domain;

		let save_button = input ~attrs:[("type","button");("value","save")] () in
		let unchanged_domain = S.l2 (fun db_dom domain_info ->
			match db_dom with
				| Some db -> Store.eq (Domain db) (Domain domain_info)
				| None -> false
		) domain_record domain_info in
		save_button#class_s "hidden" unchanged_domain;
		save_button#mechanism (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				Ui.stop event;
				let current_db = S.value db_signal in
				let new_db = Store.update current_db (Domain (S.value domain_info)) in
				log#info "Saving new DB: %s" (Store.to_json_string new_db);
				local_db#save (Store.to_json new_db);
				Lwt.return_unit
			)
		);

		domain_panel#append_all [
			child div ~cls:"panel-heading" ~children:[
				child h3 ~children: [
					(domain_is_unknown
						|> S.map (fun unknown -> if unknown then "New domain:" else "Saved domain:")
						|> Ui.text_stream);

					frag save_button;
				] ();
			] ();
			child div ~cls:"panel-body" ~children: [
				child div ~children:[
					child strong ~cls: "name" ~text: "Domain: " ();
					(domain_info |> S.map (fun i -> i.domain) |> Ui.text_stream);
				] ();

				child div ~cls:"inline" ~children:[
					child strong ~text: "Hint: " ();
					child span ~children:[
						frag hint_input;
					] ();
				] ();

				child div ~cls:"inline" ~children:[
					child strong ~text: "Length: " ();
					child span ~children:[
						frag length_input;
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

