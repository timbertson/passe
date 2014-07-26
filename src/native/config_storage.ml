open React_ext
module Json = Json_ext
let log = Logging.get_logger "local_storage"

(* A file-backed config compatible with Local_storage *)

let with_open_in file fn =
	let fd = Unix.openfile file [Unix.O_RDONLY] 0 in
	Common.finally_do Unix.close fd (fun fd ->
		let ch = Unix.in_channel_of_descr fd in
		fn ch
	)

let with_open_out file fn =
	let fd = Unix.openfile file [Unix.O_WRONLY] 0 in
	Common.finally_do Unix.close fd (fun fd ->
		let ch = Unix.out_channel_of_descr fd in
		fn ch
	)

let empty = `Assoc []

class provider path =
	let read () : Json.obj =
		try with_open_in path (fun f ->
			Json.from_channel ~fname:path f |> Json.as_object
		)
		with Unix.Unix_error (Unix.ENOENT, _, _) -> empty
	in
	let current_contents = ref (Lazy.from_fun read) in

	let write (contents : Json.obj) =
		let tmp_path = (path ^ ".tmp") in
		with_open_out tmp_path (fun f ->
			Json.pretty_to_channel ~std:true f (contents:>Json.json)
		);
		Unix.rename tmp_path path;
		current_contents := lazy contents
	in
	object
	inherit Config.base_provider
	method _erase_all = ()
	method get key = Lazy.force !current_contents |> Json.get_field key

	method set key value =
		Some (Lazy.force !current_contents)
			|> Json.set_field key value
			|> write

	method delete key =
		Some (Lazy.force !current_contents)
			|> Json.without_field key
			|> Option.default empty
			|> write
end

let erase_all impl = impl#erase_all
let record ~(impl:provider) key : Config.record = impl#create key
