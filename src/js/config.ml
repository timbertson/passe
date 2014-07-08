open React
module J = Json_ext

let log = Logging.get_logger "config"

let opt_as_object : J.json option -> J.obj option = Option.map J.as_object

let access key root = root |> Option.bind (J.get_field key)

class child parent key =
	let s = parent#signal |> S.map opt_as_object in
	let signal = lazy (s |> S.map (access key)) in
	object (self)
	method get = S.value s |> access key
	method save ?(step:step option) newval = parent#save ?step (S.value s |> J.set_field key newval)
	method delete ?(step:step option) () : unit =
		log#info "deleting config key %s from %a"
			key
			(Option.print J.print) ((S.value s):>J.json option);
		match (S.value s |> J.without_field key) with
		| Some c -> parent#save ?step c
		| None -> parent#delete ?step
	method signal = Lazy.force signal
end


type t = {
	field: string -> child
}

let build (impl:Local_storage.provider) =
	let root = Local_storage.record ~impl "config" in
	{
		field = fun key -> new child root key
	}

let persistent = lazy (build (Lazy.force Local_storage.persistent))
let ephemeral = lazy (build (Lazy.force Local_storage.ephemeral))
