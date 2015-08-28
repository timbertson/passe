module Json = Yojson.Safe
module type Sig = sig
	type response =
		| OK of Json.json
		| Unauthorized of string option
		| Failed of int * string * (Yojson.Safe.json option)


	type url = [
		| `Absolute of Uri.t
		| `Path of string list
		]

	val path : string list -> url
	val post_json : ?token:Json.json -> data:Json.json -> url -> response Lwt.t
	val get_json : ?token:Json.json -> url -> response Lwt.t

	val root_url : Uri.t ref
	(* val canonicalize : root:Uri.t -> url *)
end

