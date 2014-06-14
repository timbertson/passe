open Common
let log = Logging.get_logger "store"
exception InvalidFormat of string
let raise_invalid_format fmt = Printf.ksprintf (fun err -> raise (InvalidFormat err)) fmt

module J = struct
	include Yojson.Safe
	let print = Yojson.Safe.pretty_to_channel ~std:false
	let print_str () = pretty_to_string ~std:false
	let find_safe fn l = try Some (List.find fn l) with Not_found -> None
end

type digest =
	| MD5
	| SHA1

type record_type = [
	| `Alias
	| `Domain
]

let string_of_record_type t = match t with
	| `Alias -> "alias"
	| `Domain -> "domain"

let record_type_of_string s = match s with
	| "alias" -> `Alias
	| "domain" -> `Domain
	| _ -> raise_invalid_format "Expected alias or domain, got %s" s

let default_length = 10 (* TODO: put in DB *)
let default_digest = MD5 (* TODO: put in DB *)

type domain = {
	domain: string;
	hint: string option;
	length: int;
	digest: digest;
}

type alias = {
	alias: string;
	destination: string;
}

type record =
	| Domain of domain
	| Alias of alias

let id_of record = match record with
	| Domain d -> d.domain
	| Alias a -> a.alias

let record_for key record = key = (id_of record)

type t = record list

let get domain db : record option =
	try
		Some (db |> List.find (record_for domain))
	with Not_found -> None

let lookup domain db : domain option =
	let rec _lookup domain =
		let record = db |> List.find (record_for domain) in
		match record with
			| Alias a -> _lookup a.destination
			| Domain d -> d
	in
	try Some (_lookup domain)
	with Not_found -> None

let string_of_digest = function
	| MD5 -> "md5"
	| SHA1 -> "sha1"

let digest_of_string = function
	| "md5" -> MD5
	| "sha1" -> SHA1
	| s -> raise_invalid_format "unsupported digest type: %s" s

module Format = struct
	let current_version = 1
	type 't field = {
		key: string;
		getter: (J.json option -> 't);
		setter: ('t -> J.json option)
	}
	let flatten_opt v = match v with Some v -> v | None -> `Null
	let fail expected_type j =
		raise_invalid_format "Expected %s, got %a"
			expected_type
			J.print_str j

	let get_string j = match j with `String s -> s | j -> fail "string" j
	let set_string s = Some (`String s)
	let json_string s : J.json = `String s

	let get_int j = match j with `Int i -> i | j -> fail "number" j
	let set_int i = Some (`Int i)

	(* let default getter v = match v with *)
	(* 	| None -> default *)
	(* 	| Some v -> getter v *)

	let optional fn v = match v with
		| None -> None
		| Some v -> Some (fn v)

	let mandatory fn (v:J.json option) = fn @@ match v with
		| Some v -> v
		| None -> `Null

	let string_key k = { key=k; getter=mandatory get_string; setter=set_string }

	let hint = {key="hint"; getter=optional get_string; setter=optional json_string}
	let length = {key="length"; getter=mandatory get_int; setter=set_int }
	let digest = { key="digest";
		getter = mandatory (fun d -> get_string d |> digest_of_string);
		setter = (fun d -> string_of_digest d |> set_string)
	}
	let destination = string_key "destination"
	let record_type = {key="type";
		getter = mandatory (fun t -> get_string t |> record_type_of_string);
		setter = (fun t -> string_of_record_type t |> set_string)
	}

	let parse_field field pairs =
		let json = J.find_safe (fun (k,v) -> k = field.key) pairs in
		try
			field.getter (Option.map snd json)
		with InvalidFormat err ->
			raise (InvalidFormat ("Error getting " ^ field.key ^ ": " ^ err))
	
	let store_field field v = let v = field.setter v in
		v |> Option.map (fun v -> (field.key, v))

end



let json_of_opt_string v =
	match v with
		| Some v -> `String v
		| None -> `Null

let build_assoc pairs : J.json =
	`Assoc (pairs |> List.fold_left (fun acc v ->
		match v with None -> acc | Some v -> v::acc) [])

let json_of_record r : (string * J.json) =
	let open Format in
	match r with
	| Alias a -> (a.alias, build_assoc [
		store_field record_type `Alias;
		store_field destination a.destination
	])
	| Domain d -> (d.domain, build_assoc [
		store_field record_type `Domain;
		store_field hint d.hint;
		store_field length d.length;
		store_field digest d.digest
	])

let to_json : t -> J.json = fun db ->
	`Assoc (db |> List.map json_of_record)

let to_json_string db = J.to_string (to_json db)

let parse_record : (string * J.json) -> record = fun (id, r) ->
	let open Format in
	log#info "Parsing: %a" J.print r;
	match r with
		| `Assoc pairs -> begin
			match parse_field Format.record_type pairs with
				| `Alias -> Alias { alias=id; destination=parse_field destination pairs }
				| `Domain -> Domain {
					domain=id;
					hint = parse_field hint pairs;
					length = parse_field length pairs;
					digest = parse_field digest pairs
				}
			end
		| _ ->
			log#error "can't parse!";
			raise (InvalidFormat "can't parse record")

let parse_json json : t =
	match json with
		| `Assoc (records:(string * J.json) list) ->
				(records |> List.map parse_record)
		| _ -> raise (InvalidFormat "Expected toplevel object")

let parse : string -> (string, t) either = fun str ->
	try
		Right (J.from_string str |> parse_json)
	with
		| InvalidFormat str -> Left str
		| Yojson.Json_error str -> Left str

