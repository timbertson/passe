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
	let initial = local_db#get |> Store.parse_json in
	let signal, update = S.create initial in
	local_db#watch (fun db -> update (Store.parse_json db));
	signal

let optional_signal_content : ('a -> #Dom.node Js.t) -> 'a option React.signal -> Dom.node Js.t signal =
	fun f signal ->
		let empty = (document##createComment(s"placeholder"):>Dom.node Js.t) in
		signal |> S.map (fun value -> match value with
			| Some value -> ((f value):>Dom.node Js.t)
			| None -> empty
		)

let db_editor () : #Dom_html.element Ui.widget =
	let textarea = Ui.textArea document in
	let error_text, set_error_text = S.create None in

	textarea#attr "rows" "10";
	textarea#attr "cols" "60";
	textarea#mechanism (fun elem ->
		log#info "Mechanism is running!";
		elem##value <- local_db#get_str;
		elem##focus();
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
		let div = Dom_html.createDiv document in
		div##classList##add(s"error");
		Dom.appendChild div (document##createTextNode(s err));
		div
	) in
	let error_elem = Ui.stream error_dom_stream in
	let result = Ui.div document in
	result#append error_elem;
	result#append textarea;
	result

let password_form () : #Dom_html.element Ui.widget =
	let doc = document in
	let form = Ui.form doc in
	let domain_label = Ui.label doc in

	let domain_section = Ui.div doc in
	domain_section#attr "class" "test";

	let password_section = Ui.div doc in
	password_section#attr "class" "test";

	let submit_button = Ui.input doc in
	submit_button#attr "type" "submit";
	submit_button#attr "class" "btn btn-primary";
	submit_button#append @@ Ui.text "generate" doc;

	let domain_sig, set_domain_sig = S.create "" in

	domain_label#append @@ Ui.text "domain:" doc;
	let domain_input = Ui.element (fun () -> createInput doc ~_type:(s"text") ~name:(s"domain")) in
	domain_input#mechanism (fun elem ->
		log#info "domain watcher running";
		Lwt_js_events.inputs elem (fun event _ ->
			let contents = elem##value |> Js.to_string in
			log#info "got domain: %s" contents;
			set_domain_sig contents;
			Lwt.return_unit
		)
	);
	let password_label = Ui.label doc in
	password_label#append @@ Ui.text "password:" doc;
	let password_input = Ui.element (fun () -> createInput doc ~_type:(s"password") ~name:(s"password")) in

	let domain_validity = domain_sig |> S.map (fun dom ->
		let db = S.value db_signal in
		let found = Store.lookup dom db in
		match found with
			| None -> "[new...]"
			| Some _ -> "ok"
	) |> Ui.text_stream in

	domain_section#append domain_label;
	domain_section#append domain_input;
	domain_section#append domain_validity;

	let current_password, set_current_password = S.create None in

	let password_display = current_password |> optional_signal_content (fun p ->
		let container = Dom_html.createDiv document in
		container##setAttribute (s "class", s "password-display");
		Dom.appendChild container document##createTextNode(s p);
		container
	) |> Ui.stream in

	let invalidate_password = fun elem ->
		Lwt_js_events.inputs elem (fun event _ ->
			set_current_password None;
			Lwt.return_unit
		) in

	password_section#append password_label;
	password_section#append password_input;
	password_section#append password_display;

	password_input#mechanism invalidate_password;
	domain_input#mechanism invalidate_password;

	form#append domain_section;
	form#append password_section;
	form#append submit_button;
	form#mechanism (fun elem ->
		Lwt_js_events.submits elem (fun event _ ->
			Ui.stop event;
			log#info "form submitted";
			let db = S.value db_signal in
			let field_values = Form.get_form_contents elem |> StringMap.from_pairs in
			let domain_text = StringMap.find "domain" field_values in

			(* TODO: store & retrieve defaults in DB *)
			let domain = match Store.lookup domain_text db with
				| Some d -> d
				| None -> Store.({
					domain=domain_text;
					hint=None;
					length= 10;
					digest = MD5;
				})
			in
			let password = StringMap.find "password" field_values in
			let password = Password.generate ~domain password in
			log#warn "generated: %s" password;
			set_current_password (Some password);
			(* TODO: highlight generated password *)
			Lwt.return_unit
		)
	);
	form

let show_form (container:Dom_html.element Js.t) =
	let doc = document in
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	log#info "Hello container!";
	(* Ui.withContent container form (fun _ -> *)
	let all_content = Ui.div doc in
	all_content#append @@ db_editor ();
	all_content#append @@ password_form ();
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
	let listener = ref null in
	listener := Opt.return @@ Dom_events.listen
		window
		(Event.make "DOMContentLoaded")
		(fun _ _ ->
			Opt.iter !listener Dom_events.stop_listen;
			main ();
			false
		)

