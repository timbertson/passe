(*
[Exposed=Window,SharedWorker]
interface ApplicationCache : EventTarget {

	// update status
	const unsigned short UNCACHED = 0;
	const unsigned short IDLE = 1;
	const unsigned short CHECKING = 2;
	const unsigned short DOWNLOADING = 3;
	const unsigned short UPDATEREADY = 4;
	const unsigned short OBSOLETE = 5;
	readonly attribute unsigned short status;

	// updates
	void update();
	void abort();
	void swapCache();

	// events
					 attribute EventHandler onchecking;
					 attribute EventHandler onerror;
					 attribute EventHandler onnoupdate;
					 attribute EventHandler ondownloading;
					 attribute EventHandler onprogress;
					 attribute EventHandler onupdateready;
					 attribute EventHandler oncached;
					 attribute EventHandler onobsolete;
};




// sample code:

// Check if a new cache is available on page load.
window.addEventListener('load', function(e) {

	window.applicationCache.addEventListener('updateready', function(e) {
		if (window.applicationCache.status == window.applicationCache.UPDATEREADY) {
			// Browser downloaded a new app cache.
			if (confirm('A new version of this site is available. Load it?')) {
				window.location.reload();
			}
		} else {
			// Manifest didn't changed. Nothing new to server.
		}
	}, false);

}, false);

AppCache events

As you may expect, additional events are exposed to monitor the cache's state. The browser fires events for things like download progress, updating the app cache, and error conditions. The following snippet sets up event listeners for each type of cache event:

function handleCacheEvent(e) {
	//...
}

function handleCacheError(e) {
	alert('Error: Cache failed to update!');
};

// Fired after the first cache of the manifest.
appCache.addEventListener('cached', handleCacheEvent, false);

// Checking for an update. Always the first event fired in the sequence.
appCache.addEventListener('checking', handleCacheEvent, false);

// An update was found. The browser is fetching resources.
appCache.addEventListener('downloading', handleCacheEvent, false);

// The manifest returns 404 or 410, the download failed,
// or the manifest changed while the download was in progress.
appCache.addEventListener('error', handleCacheError, false);

// Fired after the first download of the manifest.
appCache.addEventListener('noupdate', handleCacheEvent, false);

// Fired if the manifest file returns a 404 or 410.
// This results in the application cache being deleted.
appCache.addEventListener('obsolete', handleCacheEvent, false);

// Fired for each resource listed in the manifest as it is being fetched.
appCache.addEventListener('progress', handleCacheEvent, false);

// Fired when the manifest resources have been newly redownloaded.
appCache.addEventListener('updateready', handleCacheEvent, false);
*)

open Js
open Passe

type cache_event = [
	| `Updateready
]

class type application_cache_event = object
	inherit Dom_html.event
end

(* type application_cache_status = *)
(*	 | Uncached *)
(*	 | Checking *)
(*	 | Downloading *)
(*	 | Idle *)

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
	log#info "Host: %s" (Js.to_string hostname);
	match Js.to_string hostname with
		| "localhost" -> begin
			let timeout = ref 1 in
			while_lwt true; do
				log#info "resetting app cache timeout to 1s";
				let healthy = ref false in

				let open Lwt in
				Lwt.pick [
					(
					Lwt.join [
						(
							lwt (_:Server.response) = Server.get_json (Server.path ["hold"]) in
							(* log#info "server hold exited..."; *)
							if !healthy then (
								log#info("resetting app cache timeout to 1s");
								timeout := 1;
								return_unit
							) else (
								Lwt_js.sleep 10.0;
							)
						);(
							lwt () = Lwt_js.sleep 10.0 in
							(* log#info "/hold has taken longer than 10s"; *)
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
