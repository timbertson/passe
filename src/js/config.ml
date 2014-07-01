open React
module J = Json_ext

let local = new Local_storage.record "config"

let s, update_s = S.create (local#get)
let () = local#watch (fun v -> update_s (Some v))

let access key root = root |> Option.bind (J.get_field key)

class child key =
	let signal = lazy (s |> S.map (access key)) in
	object (self)
	method get = S.value s |> access key
	method save newval = local#save(S.value s |> J.set_field key newval)
	method delete = match (S.value s |> J.without_field key) with
		| Some c -> local#save c
		| None -> local#delete
	method signal = Lazy.force signal
end


let field key = new child key
