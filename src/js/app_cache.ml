open Js
open Passe
open Passe_js

type cache_event = [
	| `Updateready
]

class type application_cache_event = object
	inherit Dom_html.event
end

class type application_cache = object
	inherit Dom_html.eventTarget
	method update : unit meth
end


let application_cache : application_cache t = Js.Unsafe.variable "applicationCache"
let update_ready_event = Dom.Event.make "updateready"
let update_monitor fn =
	Lwt_js_events.seq_loop
		(Lwt_js_events.make_event update_ready_event)
		application_cache
		(fun (_:application_cache_event Js.t) _ ->
			fn ()
		)

let update () =
	try
		application_cache##update()
	with _ -> ()
