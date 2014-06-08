module J = Yojson.Safe

exception InvalidFormat of string

let raise_invalid_format fmt =
	Printf.ksprintf (fun s -> raise (InvalidFormat s)) fmt

type digest =
	| MD5
	| SHA1

let default_length = 10 (* TODO: put in DB *)
let default_digest = MD5 (* TODO: put in DB *)


type domain = {
	id: string;
	hint: string option;
	length: int;
	digest: digest;
}

type alias = {
	id: string;
	destination: string;
}

type record =
	| Domain of domain
	| Alias of alias

type t = record list

let string_of_digest = function
	| MD5 -> "md5"
	| SHA1 -> "sha1"

let digest_of_string = function
	| "md5" -> MD5
	| "sha1" -> SHA1
	| s -> raise_invalid_format "unsupported digest type: %s" s

let json_of_opt_string v =
	match v with
		| Some v -> `String v
		| None -> `Null

let json_of_record r = match r with
	| Alias a -> (a.id, `String a.destination)
	| Domain d -> (d.id, `Assoc [
		("hint", json_of_opt_string d.hint);
		("length", `Int d.length);
		("digest", `String (string_of_digest d.digest));
	])

let to_json : t -> string = fun db ->
	J.to_string (
		`Assoc (db |> List.map json_of_record)
	)


let parse_record : (string * J.json) -> record = fun (id, r) ->
	match r with
		| `String alias -> (Alias {id=id; destination=alias})
		| `Assoc pairs -> begin
			let find_safe fn l = try Some (List.find fn l) with Not_found -> None in
			let find_value key =
				pairs |> find_safe (fun (k,v) -> k = key) |> Option.map Tuple.snd
			in

			let find_string k = find_value k |> Option.map (fun j -> match j with
				| `String s -> s
				| _ -> raise (InvalidFormat "Expected string[%s], got %a") (* XXX format strings *)
			) in

			let find_int k = find_value k |> Option.map (fun j -> match j with
				| `Int s -> s
				| _ -> raise (InvalidFormat "Expected int[%s], got %a")
			) in

			Domain {
				id = id;
				hint = find_string "hint";
				length = find_int "length" |> Option.default default_length;
				digest = find_string "digest" |> Option.map digest_of_string |> Option.default default_digest;
			}
		end
		| _ -> raise (InvalidFormat "can't parse record")

let parse : string -> t = fun str ->
	let json = J.from_string str in
	match json with
		| `Assoc (records:(string * J.json) list) ->
				records |> List.map parse_record
		| _ -> raise (InvalidFormat str)


let hai () = "hello!"
