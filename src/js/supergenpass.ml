open Lwt
open Js
open Dom_html
open Common
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

let db_editor () : #Dom_html.element Ui.widget =
	let ((error_text:string option Lwt_stream.t), set_error_text) = Lwt_stream.create () in
	let set_error_text x =
		log#info "Setting error text to: %s" (match x with Some x -> "Some "^x | None -> "None");
		set_error_text (Some x) in (* never-ending stream *)
	let textarea = Ui.textArea document in
	textarea#attr "rows" "10";
	textarea#attr "cols" "60";
	textarea#mechanism (fun elem ->
		log#info "Mechanism is running!";
		elem##value <- local_db#get_str;
		elem##focus();
		Lwt_js_events.buffered_loop Lwt_js_events.input elem (fun evt rv ->
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
	let error_dom_stream : Dom.node Js.t Lwt_stream.t = error_text |> Lwt_stream.map (fun err ->
		match err with
			| Some err ->
					let div = Dom_html.createDiv document in
					div##classList##add(s"error");
					Dom.appendChild div (document##createTextNode(s err));
					(div:>Dom.node Js.t)
			| None -> (document##createComment(s"placeholder"):>Dom.node Js.t)
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

	domain_label#append @@ Ui.text "domain:" doc;
	let domain_input = Ui.element (fun () -> createInput doc ~_type:(s"text") ~name:(s"domain")) in
	let password_label = Ui.label doc in
	password_label#append @@ Ui.text "password:" doc;
	let password_input = Ui.element (fun () -> createInput doc ~_type:(s"password") ~name:(s"password")) in

	domain_section#append domain_label;
	domain_section#append domain_input;

	password_section#append password_label;
	password_section#append password_input;
	form#append domain_section;
	form#append password_section;
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

