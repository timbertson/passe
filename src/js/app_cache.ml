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

let log = Logging.get_logger "app_cache"

let poll_server () =
	let hostname = Dom_html.window##location##hostname in
	match Js.to_string hostname with
		| "localhost" -> begin
			let timeout = ref 1 in
			while_lwt true; do
				log#debug "resetting app cache timeout to 1s";
				let healthy = ref false in

				let open Lwt in
				Lwt.pick [
					(
					Lwt.join [
						(
							lwt (_:Server.response) = Server.get_json (Server.path ["hold"]) in
							log#debug "server hold exited...";
							if !healthy then (
								log#debug "resetting app cache timeout to 1s";
								timeout := 1;
								return_unit
							) else (
								Lwt_js.sleep 10.0;
							)
						);(
							lwt () = Lwt_js.sleep 10.0 in
							(* log#debug "/hold has taken longer than 10s"; *)
							healthy := true;
							return_unit
						)
					]
					) ; (
						while_lwt true; do
							(* log#debug "sleeping for %d seconds" !timeout; *)
							lwt () = Lwt_js.sleep (float_of_int !timeout) in
							(* log#debug "checking for update"; *)
							update ();
							timeout := !timeout + (max (!timeout / 2) 1);
							return_unit
						done
					)
				]
			done
		end
		| other_host -> log#info "app-cache polling disabled"; Lwt.return_unit
