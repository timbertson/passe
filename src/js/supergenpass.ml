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

let db_editor () : #Dom_html.element Ui.element =
	let ((error_text:string option Lwt_stream.t), set_error_text) = Lwt_stream.create () in
	let set_error_text x =
		log#info "Setting error text to: %s" (match x with Some x -> "Some "^x | None -> "None");
		set_error_text (Some x) in (* never-ending stream *)
	let textarea = Ui.textArea document in
	let textarea = textarea#mechanism (fun elem ->
		log#info "Mechanism is running!";
		elem##focus();
		Lwt_js_events.buffered_loop Lwt_js_events.input elem (fun evt rv ->
			let contents = elem##value |> Js.to_string in
			begin match Store.parse contents with
				| Left err ->
						set_error_text (Some err)
				| Right db ->
						set_error_text None;
						log#info "got db: %s" (Store.to_json db)
			end;
			Lwt.return_unit
		)
	) in
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
	Ui.div ~children:[
		(error_elem:>Ui.fragment);
		(textarea:>Ui.fragment)
	] document

let show_form (container:Dom_html.element Js.t) =
	let doc = document in
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	log#info "Hello container!";
	let form = createForm document in
	let domain_label = createLabel doc in

	let domain_section = createDiv doc in
	domain_section##setAttribute((s"class"), (s"test"));

	let password_section = createDiv doc in
	password_section##setAttribute((s"class"), (s"test"));

	Dom.appendChild domain_label (document##createTextNode (s"domain:"));
	let domain_input = createInput doc ~_type:(s"text") ~name:(s"domain") in
	let password_label = createLabel doc in
	Dom.appendChild password_label (document##createTextNode (s"password:"));
	let password_input = createInput doc ~_type:(s"password") ~name:(s"password") in

	Dom.appendChild domain_section domain_label;
	Dom.appendChild domain_section domain_input;
	Dom.appendChild password_section password_label;
	Dom.appendChild password_section password_input;
	Dom.appendChild form domain_section;
	Dom.appendChild form password_section;
	(* Ui.withContent container form (fun _ -> *)
	Ui.withContent container (db_editor () :>'a Ui.widget) (fun _ ->
		log#info "HELLO";
		lwt () = hold 50000 in
		log#info "FORM WOZ HERE";
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

