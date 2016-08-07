open React
open Common
module J = Json_ext

module Log = (val Logging.log_module "config")

let opt_as_object : J.json option -> J.obj option = Option.map J.as_object

let access key root = root |> Option.bind (J.get_field key)

class child parent key =
	let s = parent#signal |> S.map opt_as_object in
	let signal = lazy (s |> S.map (access key)) in
	object (self)
	method get = S.value s |> access key
	method save ?(step:step option) newval =
		Log.debug (fun m->m "saving key: %s" key);
		parent#save ?step (S.value s |> J.set_field key newval)
	method delete ?(step:step option) () : unit =
		Log.info (fun m->m "deleting config key %s from %a"
			key
			(Option.fmt J.fmt) ((S.value s):>J.json option));
		match (S.value s |> J.without_field key) with
		| Some c -> parent#save ?step c
		| None -> parent#delete ?step ()
	method signal = Lazy.force signal
end


type t = {
	field: string -> child
}

class type record_t = object
	method get : J.json option
	method save : ?step:step -> J.json -> unit
	method delete : ?step:step -> unit -> unit
	method refresh : unit
	method signal : J.json option signal
end

class type provider_t = object
	method create : string -> record_t
	method get: string -> J.json option
	method set : string -> J.json -> unit
	method delete : string -> unit
end

class record (impl:#provider_t) key =
	let get () = impl#get key in
	let signal, update_signal = S.create (get ()) in
	object (self : #record_t)
	method get = get ()

	method save ?(step:step option) v =
		impl#set key v;
		self#update ?step (Some v)

	method delete ?(step:step option) () =
		impl#delete key;
		self#update ?step None

	method refresh = self#update (self#get)

	method signal = signal

	method private update ?(step:step option) v =
		update_signal ?step v
end


class virtual base_provider =
	let cache: record StringMap.t ref = ref StringMap.empty in
	object (self : #provider_t)
	method create key =
		try
			StringMap.find key !cache
		with Not_found -> (
			let rv = new record (self:>provider_t) key in
			cache := StringMap.add key rv !cache;
			rv
		)

	method erase_all =
		self#_erase_all;
		StringMap.bindings !cache |> List.iter (fun (_k,v) -> v#refresh)

	method virtual get: string -> J.json option
	method virtual set : string -> J.json -> unit
	method virtual delete : string -> unit
	method virtual _erase_all : unit
end

let build (impl:#provider_t) =
	let root = impl#create "config" in
	{
		field = fun key -> new child root key
	}
