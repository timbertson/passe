let global_event_listener event handler =
	let doc = Dom_html.document##documentElement in
	Dom.addEventListener doc event (Dom.handler (fun e ->
		handler e;
		Js._true (* continue event *)
	))
	Js._true (* use capture *)

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
