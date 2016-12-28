let global_event_listener ?(target:#Dom_html.eventTarget Js.t option) ?capture event handler =
	let target = match target with
		| Some target -> (target:>Dom_html.eventTarget Js.t)
		| None -> (Dom_html.document##documentElement:>Dom_html.eventTarget Js.t)
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
