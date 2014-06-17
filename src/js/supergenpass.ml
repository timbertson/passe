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
		let div = Ui.div () in
		div#attr "class" "error";
		div#append @@ Ui.text err;
		(div:>Dom.node Ui.widget_t) (* XXX remove cast *)
	) in
	let error_elem = Ui.stream error_dom_stream in
	let result = Ui.div () in
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
	domain_input#mechanism (fun elem -> elem##focus(); invalidate_password elem);
	password_input#mechanism invalidate_password;

	let domain = Ui.input_signal ~events:Lwt_js_events.inputs domain_input in
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

	let form = Ui.form ~children:[
		child div ~children:[
			child label ~text:"domain:" ();
			frag domain_input;
		] ();
		child div ~children:[
			child label ~text:"password:" ();
			frag password_input;
		] ();
		child input ~cls:"btn btn-primary" ~text:"generate" ~attrs:[("type", "submit")] ();
	] () in

	let domain_display = Ui.div () in
	let () =
		let open Store in
		let open Ui in
		domain_display#class_s "unknown" domain_is_unknown;
		domain_display#append_all [
			child div ~children:[
				child span ~cls: "domain" ~text: "Domain: " ();
				(domain_info |> S.map (fun i -> i.domain) |> Ui.text_stream);
				(domain_is_unknown
					|> S.map (fun unknown -> if unknown then " [new domain]" else "")
					|> Ui.text_stream);
			] ();

			child div ~children:[
				child span ~cls: "length" ~text: "Length: " ();
				(domain_info |> S.map (fun i -> string_of_int i.length) |> Ui.text_stream);
			] ();
		];
	in


	let password_display = current_password |> optional_signal_content (fun p ->
		let container = div
			~cls:"password-display"
			~text:p
			~mechanism:(fun elem ->
				Selection.select elem;
				Lwt.return_unit
			) () in
		(container:>Dom.node Ui.widget_t)
	) |> Ui.stream in

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
	div ~children:[ frag form; frag domain_display; frag password_display ] ()

let show_form (container:Dom_html.element Js.t) =
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	log#info "Hello container!";
	(* Ui.withContent container form (fun _ -> *)
	let all_content = Ui.div () in
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

