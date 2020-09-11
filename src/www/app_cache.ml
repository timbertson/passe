open Js_of_ocaml
open Js
open Passe
open Common
module Log = (val Logging.log_module "app_cache")

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


let application_cache : application_cache t optdef = Unsafe.get Unsafe.global "applicationCache"
let update_ready_event = Dom.Event.make "updateready"
let update_monitor fn =
	Optdef.case application_cache
	(fun () ->
		Log.warn (fun m->m "applicationCache is undefined, caching disabled");
		Lwt.return_unit
	) (fun application_cache -> Lwt_js_events.seq_loop
		(Lwt_js_events.make_event update_ready_event)
		application_cache
		(fun (_:application_cache_event Js.t) _ ->
			fn ()
		)
	)

