open Common
open Re_compat
module J = Json_ext
let log = Logging.get_logger "store"
exception InvalidFormat of string
let raise_invalid_format fmt = Printf.ksprintf (fun err -> raise (InvalidFormat err)) fmt

module StringMap = struct
	include Map.Make(String)
	let from_pairs (pairs:(string * 'v) list) :'v t =
		pairs |> List.fold_left (fun map (k,v) ->
			assert (not (mem k map));
			add k v map
		) empty

	let from_values ~(key:'v -> string) (values:'v list) :'v t =
		from_pairs (values |> List.map (fun v -> (key v, v)))

	let keys map = bindings map |> List.map Tuple.fst

	let find_opt key map = try Some (find key map) with Not_found -> None
end

(* type digest = *)
(* 	| MD5 *)
(* 	| SHA1 *)

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
(* let default_digest = MD5 (* TODO: put in DB *) *)

type domain = {
	domain: string;
	hint: string option;
	suffix: string option;
	length: int;
	(* digest: digest; *)
}

let default domain = {
	domain = domain;
	hint = None;
	suffix = None;
	length = default_length;
	(* digest = default_digest; *)
}

type alias = {
	alias: string;
	destination: string;
}

type record =
	| Domain of domain
	| Alias of alias

type alias_field_change = [
	| `Alias of string
	| `Destination of string
	]
type domain_field_change = [
	| `Domain of string
	| `Hint of string option
	| `Suffix of string option
	| `Length of int
	(* | `Digest of digest *)
	]

type field_change = [domain_field_change | alias_field_change]

let rec filter_some : 'a option list -> 'a list = function
	| [] -> []
	| (Some x) :: rest -> x :: (filter_some rest)
	| None :: rest -> filter_some rest

let id_of record = match record with
	| Domain d -> d.domain
	| Alias a -> a.alias

let record_for key record = key = (id_of record)

type edit = [
	| `Domain of domain_field_change list
	| `Alias of alias_field_change list
	]

type change =
	| Edit of (string * edit)
	| Create of record
	| Delete of string

type core = {
	version: int;
	records: record StringMap.t;
}

type t = {
	core: core;
	changes: change list;
	composite: record StringMap.t Lazy.t;
}

let record_eq:record -> record -> bool = fun a b -> a = b
let core_eq a b =
	(a.version = b.version) &&
	(StringMap.equal record_eq a.records b.records)

let eq : t -> t -> bool = fun a b ->
	(core_eq a.core b.core) &&
	(a.changes = b.changes)
	(* NOTE: we don't need to compare `coposite` field,
	 * as that's derived purely from the information we
	 * _do_ compare *)



let empty_core = {version=0; records=StringMap.empty}
let empty:t = { core=empty_core; changes=[]; composite=lazy StringMap.empty }

let get domain db : record option =
	try
		Some (db |> List.find (record_for domain))
	with Not_found -> None

(* let string_of_digest = function *)
(* 	| MD5 -> "md5" *)
(* 	| SHA1 -> "sha1" *)
(*  *)
(* let digest_of_string = function *)
(* 	| "md5" -> MD5 *)
(* 	| "sha1" -> SHA1 *)
(* 	| s -> raise_invalid_format "unsupported digest type: %s" s *)

let string_of_change_type = function
	| `Edit -> "edit"
	| `Delete -> "delete"
	| `Create -> "create"

let change_type_of_string = function
	| "edit" -> `Edit
	| "delete" -> `Delete
	| "create" -> `Create
	| s -> raise_invalid_format "Expected change type, got %s" s

module Format = struct
	let current_version = 1
	type 't field = {
		key: string;
		getter: (J.json option -> 't);
		setter: ('t -> J.json option)
	}

	(* entry points for parse / serialize *)
	let parse_field field pairs =
		let json = find_safe (fun (k,v) -> k = field.key) pairs in
		try
			field.getter (Option.map snd json)
		with InvalidFormat err ->
			raise (InvalidFormat ("Error getting " ^ field.key ^ ": " ^ err))
	
	let store_field field v = let v = field.setter v in
		v |> Option.map (fun v -> (field.key, v))

	
	(* internal helpers / combinators *)
	let flatten_opt v = match v with Some v -> v | None -> `Null
	let fail_expecting expected_type j =
		raise_invalid_format "Expected %s, got %a"
			expected_type
			J.print_oneline j

	let get_string j = match j with `String s -> s | j -> fail_expecting "string" j
	let set_string s = Some (`String s)
	let json_string s : J.json = `String s

	let get_int j = match j with `Int i -> i | j -> fail_expecting "Int" j
	let set_int i = Some (`Int i)

	let optional fn v = match v with
		| None -> None
		| Some v -> Some (fn v)

	let mandatory fn (v:J.json option) = fn @@ match v with
		| Some v -> v
		| None -> `Null

	let build_assoc (pairs:(string * J.json) option list) : J.json =
		`Assoc (pairs |> List.fold_left (fun acc v ->
			match v with None -> acc | Some v -> v::acc) [])

	let string_key k = { key=k; getter=mandatory get_string; setter=set_string }

	let hint = {key="hint"; getter=optional get_string; setter=optional json_string}
	let suffix = {key="suffix"; getter=optional get_string; setter=optional json_string}
	let length = {key="length"; getter=mandatory get_int; setter=set_int }
	(* let digest = { key="digest"; *)
	(* 	getter = mandatory (fun d -> get_string d |> digest_of_string); *)
	(* 	setter = (fun d -> string_of_digest d |> set_string) *)
	(* } *)
	let destination = string_key "destination"
	let record_type = {key="type";
		getter = mandatory (fun t -> get_string t |> record_type_of_string);
		setter = (fun t -> string_of_record_type t |> set_string)
	}

	let id = {key="id"; getter = mandatory get_string; setter = set_string }
	let change_type = {key="type";
		getter = mandatory (change_type_of_string % get_string);
		setter = set_string % string_of_change_type;
	}

	let rec unioned_getter (options:(J.json -> 't option) list) (value:J.json) : 't =
		match options with
			| conv::options -> (
				match conv value with
					| Some result -> result
					| None -> unioned_getter options value
			)
			| [] -> raise_invalid_format "couldn't parse value %a" J.print_str value


	let parse_record : (string * J.json) -> record = fun (id, r) ->
		log#debug "Parsing: %s (%s)" id (J.to_string r);
		match r with
			| `Assoc pairs -> begin
				match parse_field record_type pairs with
					| `Alias -> Alias { alias=id; destination=parse_field destination pairs }
					| `Domain -> Domain {
						domain=id;
						hint = parse_field hint pairs;
						suffix = parse_field suffix pairs;
						length = parse_field length pairs;
						(* digest = parse_field digest pairs *)
					}
				end
			| _ ->
				log#error "can't parse!";
				raise (InvalidFormat "can't parse record")

	let json_pair_of_record r : (string * J.json) =
		match r with
		| Alias a -> (a.alias, build_assoc [
			store_field record_type `Alias;
			store_field destination a.destination
		])
		| Domain d -> (d.domain, build_assoc [
			store_field record_type `Domain;
			store_field hint d.hint;
			store_field suffix d.suffix;
			store_field length d.length;
			(* store_field digest d.digest *)
		])
	let json_of_record r = `Assoc [json_pair_of_record r]

	let version = {key="version"; getter=mandatory get_int; setter=set_int}
	let records : record StringMap.t field = {key="records";
		getter=mandatory (function
			| `Assoc pairs ->
					pairs
					|> List.map(parse_record)
					|> StringMap.from_values ~key:id_of
			| j -> fail_expecting "Object" j);
		setter=(fun r ->
			let pairs = StringMap.bindings r
				|> List.map (json_pair_of_record % Tuple.snd)
			in
			Some (`Assoc pairs));
	}

	let core_of_json = (function
			| (`Assoc fields) -> {
					version = parse_field version fields;
					records = parse_field records fields;
				}
			| j -> fail_expecting "Object" j
	)
	let json_of_core core = build_assoc [
		store_field version core.version;
		store_field records core.records;
	]

	let core = {key="core";
		setter = (fun core -> Some (json_of_core core));
		getter = (function
			| Some json -> core_of_json json
			| None -> empty_core
		)
	}

	let create_value = {key="value";
		getter = mandatory (function `List [`String id; value] -> parse_record (id, value) | _ -> assert false);
		setter = (fun r -> let k,v = json_pair_of_record r in Some (`List [`String k; v]));
	}

	module FieldChange = struct
		let domain = {key="domain"; getter = mandatory get_string; setter=set_string}
		let alias = {key="alias"; getter = mandatory get_string; setter=set_string}
	end

	let attempt (field: 't field) (conv:'t -> 'r) (value:J.json) : 'r option = match value with
		| `List [`String tag ; value] when tag = field.key -> (
			Some (field.getter (Some value) |> conv)
		)
		| _ -> None
	
	let tag field value =
		`List [`String field.key; field.setter value |> Option.default `Null]

	let domain_field_change_of_json json :domain_field_change = (unioned_getter [
		attempt FieldChange.domain (fun x -> `Domain x);
		attempt hint (fun x -> `Hint x);
		attempt suffix (fun x -> `Suffix x);
		attempt length (fun x -> `Length x);
		(* attempt digest (fun x -> `Digest x); *)
	]) json

	let alias_field_change_of_json json :alias_field_change = (unioned_getter [
		attempt FieldChange.alias (fun x -> `Alias x);
		attempt destination (fun x -> `Destination x);
	]) json

	let json_of_field_change = (function
		| `Domain x      -> tag FieldChange.domain x
		| `Alias x       -> tag FieldChange.alias x
		| `Destination x -> tag destination x
		| `Hint x        -> tag hint x
		| `Suffix x      -> tag suffix x
		| `Length x      -> tag length x
		(* | `Digest x      -> tag digest x *)
	)

	let edit : edit field = {key="edit";
		getter = mandatory (function
			| `List [`String typ; `List changes] -> (
					match record_type_of_string typ with
					| `Domain -> `Domain (List.map domain_field_change_of_json changes)
					| `Alias -> `Alias (List.map alias_field_change_of_json changes)
			)
			| j -> fail_expecting "a pair" j
		);
		setter = (fun v ->
			let typ, (changes:field_change list) = match v with
			| `Domain changes -> `Domain, (changes:>field_change list)
			| `Alias changes -> `Alias, (changes:>field_change list)
			in
			Some (`List [
				`String (string_of_record_type typ);
				`List (List.map json_of_field_change changes)
			])
		);
	}

	let change_of_json = function
		| `Assoc pairs -> (
				match parse_field change_type pairs with
				| `Edit -> Edit (
						parse_field id pairs,
						parse_field edit pairs)
				| `Create -> Create (parse_field create_value pairs)
				| `Delete -> Delete (parse_field id pairs)
			)
		| j -> fail_expecting "List of pairs" j

	let json_of_change change =
		match change with
		| Edit (_id, _edit) -> build_assoc [
				store_field id _id;
				store_field change_type `Edit;
				store_field edit _edit;
			]
		| Create record -> build_assoc [
				store_field change_type `Create;
				store_field create_value record;
			]
		| Delete _id -> build_assoc [
				store_field change_type `Delete;
				store_field id _id;
			]

	let json_of_changes changes = (`List (changes |> List.map json_of_change))
	let changes = {key="changes";
		setter=(fun changes -> Some (json_of_changes changes));
		getter=(function
			| Some (`List l) -> List.map change_of_json l
			| Some j -> fail_expecting "List" j
			| None -> []
		)
	}
	let changes_of_json c = changes.getter (Some c)
end



let json_of_opt_string v =
	match v with
		| Some v -> `String v
		| None -> `Null


let to_json : t -> J.json = fun db ->
	let open Format in
	build_assoc [
		store_field core db.core;
		store_field changes db.changes;
	]

let to_json_string = J.to_string % to_json

let json_string_of_domain d = J.to_string (
	Format.json_of_record (Domain d))

let apply_changes core changes : record StringMap.t =
	let apply_domain_edit edit domain =
		match edit with
		| `Domain x -> {domain with domain=x}
		| `Hint   x -> {domain with hint=x}
		| `Suffix x -> {domain with suffix=x}
		| `Length x -> {domain with length=x}
		(* | `Digest x -> {domain with digest=x} *)
	in

	let apply_alias_edit edit alias =
		match edit with
		| `Alias       x -> {alias with alias=x}
		| `Destination x -> {alias with destination=x}
	in

	let apply_change current = function
		| Create record ->
			if (StringMap.mem (id_of record) current) then
				log#warn "creation of %a replaces existing entry"
				J.print (Format.json_of_record record)
			;
			StringMap.add (id_of record) record current
		| Delete id ->
			if not (StringMap.mem id current) then
				log#warn "dropping deletion of %s" id
			;
			StringMap.remove id current
		| Edit (id, edit) ->
			match StringMap.find_opt id current with
			| None -> log#warn "dropping edits to %s (no such record)" id; current
			| Some existing -> (
				let new_ = match edit, existing with
				| `Domain edits, Domain record ->
						Some (Domain (List.fold_right apply_domain_edit edits record))

				| `Alias edits, Alias record ->
						Some (Alias (List.fold_right apply_alias_edit edits record))

				| _ -> log#warn "dropping edits on mismatching types for %s" id; None
				in
				match new_ with
				| Some new_ -> StringMap.add (id_of new_) new_ (StringMap.remove id current)
				| None -> current
			)
	in

	let rec inner current = function
		| [] -> current
		| change::remaining_changes ->
			inner (apply_change current change) remaining_changes
	in
	inner core.records changes


let build_t core changes =
	{
		core = core;
		changes = changes;
		composite = lazy (apply_changes core changes)
	}

let parse_json json : t =
	let open Format in
	match json with
		| `Assoc pairs ->
			build_t
				(parse_field core pairs)
				(parse_field changes pairs)
		| _ -> raise (InvalidFormat "Expected toplevel object")

let parse : string -> (string, t) either = fun str ->
	try
		Right (J.from_string str |> parse_json)
	with
		| InvalidFormat str -> Left str
		| Yojson.Json_error str -> Left str

let rec take n l =
	if n = 0 then []
	else match l with
		| x::xs -> x :: (take (n-1) xs)
		| [] -> []

let get_records (db:t) = db.composite |> Lazy.force

let lookup domain (db:t) : domain option =
	let rec _lookup domain =
		let record = get_records db |> StringMap.find domain in
		match record with
			| Alias a -> _lookup a.destination
			| Domain d -> d
	in
	try Some (_lookup domain)
	with Not_found -> None


let keys_like (db:t) query =
	let q_re = Str.regexp_string query in
	get_records db
		|> StringMap.keys
		|> List.filter (Str.contains q_re)
		|> List.sort (compare) |> take 5

let update ~(db:t) ~(original:record option) (updated:record option) =
	log#info "modifying %a -> %a"
		(Option.print J.print) (Option.map Format.json_of_record original)
		(Option.print J.print) (Option.map Format.json_of_record updated);

	let edit_change = function
		| (_, `Domain []) -> []
		| (_, `Alias []) -> []
		| edit -> [Edit edit]
	in

	let changes = match original, updated with
		| None, None -> []
		| None, Some updated -> [Create updated]
		| Some orig, None -> [Delete (id_of orig)]

		| Some (Domain orig), Some (Domain u) -> edit_change (
				orig.domain,
				`Domain (filter_some [
					Option.cond (orig.domain <> u.domain) (`Domain u.domain);
					Option.cond (orig.hint <> u.hint) (`Hint u.hint);
					Option.cond (orig.suffix <> u.suffix) (`Suffix u.suffix);
					Option.cond (orig.length <> u.length) (`Length u.length);
					(* Option.cond (orig.digest <> u.digest) (`Digest u.digest); *)
				])
			)
		| Some (Alias orig), Some (Alias u) -> edit_change (
				orig.alias,
				`Alias (filter_some [
					Option.cond (orig.alias <> u.alias) (`Alias u.alias);
					Option.cond (orig.destination <> u.destination) (`Destination u.destination);
				])
			)
		(* type change is handled by just deleting / creating *)
		| Some (Domain _ as orig), Some (Alias _ as updated)
		| Some (Alias _ as orig), Some (Domain _ as updated) ->
				[Delete (id_of orig); Create updated]
		in
	{db with changes=db.changes @ changes}

let rec drop n lst =
	if n <= 0 then lst
	else match lst with
		| [] -> []
		| head::tail -> drop (n-1) tail

let rec take n lst =
	if n <= 0 then []
	else match lst with
		| [] -> []
		| head::tail -> head :: (take (n-1) tail)

let drop_applied_changes ~from ~new_core applied_changes =
	(* if we ensure changes are _always_ appended,
	* this is totally safe: *)
	assert ((take (List.length applied_changes) from.changes) = applied_changes);
	let missed_changes = drop (List.length applied_changes) from.changes in
	build_t new_core missed_changes

