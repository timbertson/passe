open Js_of_ocaml

let global_event_listener ?(target:#Dom_html.eventTarget Js.t option) ?capture event handler =
	let target = match target with
		| Some target -> (target:>Dom_html.eventTarget Js.t)
		| None -> (Dom_html.document##.documentElement:>Dom_html.eventTarget Js.t)
	in
	let use_capture = match capture with
		| Some x -> x
		| None -> true
	in
	Dom.addEventListener target event (Dom.handler (fun e ->
		handler e;
		Js._true (* continue event *)
	))
	(Js.bool use_capture)

let with_global_listeners builder =
	let events = ref [] in
	let create _ =
		events := !events @ (builder ())
	in
	let destroy _elem =
		!events |> List.iter (Dom_html.removeEventListener);
		events := []
	in
	fun node -> Vdoml.Ui.hook ~create ~destroy node

let eqeqeq (type a) (type b): a Js.t -> b Js.t -> bool = fun a b ->
(* let eqeqeq: 'a 'b. 'a Js.t -> 'b Js.t -> bool Js.t = *)
	let open Js.Unsafe in
	let fn = js_expr "(function(a, b) { return a == b; })" in
	fun_call fn [|inject a; inject b|] |> Js.to_bool
