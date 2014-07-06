open React
module J = Json_ext

let local = Local_storage.record "config"
let log = Logging.get_logger "config"

let opt_as_object : J.json option -> J.obj option = Option.map J.as_object
let s, update_s = S.create (local#get |> opt_as_object)
let () = local#watch (fun v -> update_s (opt_as_object v))

let access key root = root |> Option.bind (J.get_field key)

class child key =
	let signal = lazy (s |> S.map (access key)) in
	object (self)
	method get = S.value s |> access key
	method save newval = local#save(S.value s |> J.set_field key newval)
	method delete =
		log#info "deleting config key %s from %a"
			key
			(Option.print J.print) ((S.value s):>J.json option);
		match (S.value s |> J.without_field key) with
		| Some c -> local#save c
		| None -> local#delete
	method signal = Lazy.force signal
end


let field key = new child key
