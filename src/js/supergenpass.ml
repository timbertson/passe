open Lwt
open Js
open Dom_html

let s = Js.string

let () = Console.console##log (s Messages.hello)
let caml_ml_output_char c = Console.log ("char: " ^ c)


exception Fail
exception AssertionError of string

let check cond = Printf.ksprintf (function s ->
	if cond then () else raise (AssertionError s)
	)

let hold duration =
	let setTimeout = Js.Unsafe.variable "setTimeout" in

	let condition = Lwt_condition.create () in
	Js.Unsafe.fun_call setTimeout [|
		Unsafe.inject (Lwt_condition.signal condition);
		Unsafe.inject duration
	|];
	lwt () = Lwt_condition.wait condition in
	Console.log "Delay!";
	Lwt.return_unit

let show_form (container:Dom_html.element Js.t) =
	let doc = document in
	Console.log ("hasDChil?" ^ (container##firstChild |> Opt.test |>
	string_of_bool));
	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);
	Console.log "Hello container!";
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
	Dom.appendChild container form;
	()

let main () = Lwt.async (fun () ->
	try_lwt (
		let main_elem = (document##getElementById (s"main")) in
		check (Opt.test main_elem) "main_elem not found!";
		Opt.get main_elem (fun _ -> raise Fail);
		Opt.iter main_elem show_form;
		try_lwt
			lwt () = hold 1000 in
			Console.log "success!";
			return_unit
		with Not_found ->
			Console.log "failed :/"
		;
		Digest.string "sekret" |> Digest.to_hex |> Console.log;
		return_unit
	) with e -> (
		Console.error ("Error: " ^ (Printexc.to_string e) ^ "\n" ^
		(Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string));
		return_unit
	)
)

let () =
	let listener = ref null in
	let listener = return @@ Dom_events.listen
		window
		(Event.make "DOMContentLoaded")
		(fun _ _ ->
			Opt.iter !listener Dom_events.stop_listen;
			(* Dom_events.stop_listen !listener; *)
			Console.log ("READY!");
			main ();
			false
		)
	in ()

