open Js_of_ocaml
open Js
open Passe
module Log = (val Logging.log_module "app_cache")

module Promise = struct
	class type ['a] promise = object
		method _then: ('a t -> 'b t) -> 'b promise t meth
		method _catch: (error t -> unit) -> unit promise t meth
	end
end

module ServiceWorker = struct
	class type setting = object
		method scope : js_string t prop
	end

	let navigator = Unsafe.variable "navigator"

	let instance = (Unsafe.get navigator "serviceWorker" |> def)
	
	let scope s = (Unsafe.obj [| "scope", Unsafe.inject (string s) |])
	
	let register serviceWorker service setting : unit Promise.promise t =
		Unsafe.meth_call serviceWorker "register" [| Unsafe.inject service; setting; |]
end

let install () =
	Optdef.iter ServiceWorker.instance (fun sw ->
		let register = ServiceWorker.register sw "/service_worker.js" (ServiceWorker.scope "/") in
		(register##_then(fun x ->
			Log.info (fun m->m"Registered serviceWorker");
			x
		))##_catch(fun err ->
			Log.err (fun m->m"Error registering serviceWorker: %s" (Js.to_string err##toString));
			()
		) |> ignore
	)
