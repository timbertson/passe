open Lwt
open Js
open Dom_html
open Common
open Lwt_react
module J = Yojson.Safe

let s = Js.string
let log = Logging.get_logger "main"

exception Fail

let check cond = Printf.ksprintf (function s ->
	if cond then () else raise (AssertionError s)
	)

let hold duration =
	let setTimeout = Js.Unsafe.variable "setTimeout" in

	let condition = Lwt_condition.create () in
	let () = Js.Unsafe.fun_call setTimeout [|
		Unsafe.inject (Lwt_condition.signal condition);
		Unsafe.inject duration
	|] in
	lwt () = Lwt_condition.wait condition in
	Lwt.return_unit

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

	let domain = Ui.input_signal ~events:Lwt_js_events.inputs domain_input in
	let empty_domain = domain |> S.map (fun d -> d = "") in
	let domain_record = S.l2 (fun db dom -> log#info "looking up!"; Store.lookup dom db) db_signal domain in
	let domain_info = S.l2 (fun domain text ->
		match domain with
			| Some d -> d
			| None -> Store.({
				domain=text;
				hint=None;
				length= 10;
				digest = MD5;
			})
	) domain_record domain in
	let domain_is_unknown = (S.map Option.is_none domain_record) in

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


	let form = Ui.form ~cls:"form-horizontal" ~attrs:(["role","form"]) ~children:[
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
	] () in

	let domain_panel = Ui.div ~cls:"domain-info panel" () in
	let () =
		let open Store in
		let open Ui in
		domain_panel#class_s "unknown" domain_is_unknown;
		domain_panel#class_s "hidden" empty_domain;
		domain_panel#append_all [
			child div ~cls:"panel-heading" ~children:[
				child h3 ~children: [
					(domain_is_unknown
						|> S.map (fun unknown -> if unknown then "New domain:" else "Saved domain:")
						|> Ui.text_stream);
				] ();
			] ();
			child div ~cls:"panel-body" ~children: [
				child div ~children:[
					child strong ~cls: "name" ~text: "Domain: " ();
					(domain_info |> S.map (fun i -> i.domain) |> Ui.text_stream);
				] ();

				child div ~children:[
					child strong ~cls: "length" ~text: "Length: " ();
					(domain_info |> S.map (fun i -> string_of_int i.length) |> Ui.text_stream);
				] ();
			] ();
		];
	in

	form#mechanism (fun elem ->
		Lwt_js_events.submits elem (fun event _ ->
			Ui.stop event;
			log#info "form submitted";
			let field_values = Form.get_form_contents elem |> StringMap.from_pairs in
			elem##querySelectorAll(s"input") |> Dom.list_of_nodeList
				|> List.iter (fun elem ->
						let input = CoerceTo.input(elem) |> non_null in
						input##blur()
				);

			(* TODO: store & retrieve defaults in DB *)
			let domain = S.value domain_info in
			let password = StringMap.find "password" field_values in
			let password = Password.generate ~domain password in
			log#warn "generated: %s" password;
			set_current_password (Some password);
			(* TODO: highlight generated password *)
			Lwt.return_unit
		)
	);
	div ~children:[
		child div ~cls:"row" ~children:[
			child div ~cls:"col-sm-8" ~children:[
				child h1 ~text:"SuperGenPass" ();
			] ();
		] ();

		child div ~cls:"row" ~children:[
			child div ~cls:"col-sm-8" ~children:[
				frag form;
			] ();
			child div ~cls:"col-sm-4" ~children:[
				frag domain_panel;
			] ();
		] ();
	] ()

let show_form (container:Dom_html.element Js.t) =
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	log#info "Hello container!";
	(* Ui.withContent container form (fun _ -> *)
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

