open Batteries
open Passe
open Common
open Lwt
module Header = Cohttp.Header
module Server = Cohttp_lwt_unix.Server
module Connection = Cohttp.Connection
module J = Json_ext

let log = Logging.get_logger "service"
let slash = Str.regexp "/"

let normpath p =
	let parts = Str.split_delim slash p in
	let try_tail = function [] -> [] | _::tail -> tail in

	let rv = ref [] in (* NOTE: parts are reversed for easy
	manipulation, reversed upon return *)
	parts |> List.iter (fun part ->
		match part with
			| "" | "." -> ()
			| ".." -> rv := try_tail !rv
			| p -> rv := p :: !rv
	);
	List.rev !rv

let content_type_key = "Content-Type"
let content_type_header v = Header.init_with content_type_key v
let json_content_type = content_type_header "application/json"
let no_cache h = Header.add h "Cache-control" "no-cache"

let respond_json ~status ~body () =
	Server.respond_string
		~headers:(json_content_type |> no_cache)
		~status ~body:(J.to_string ~std:true body) ()

let respond_unauthorized () =
	respond_json ~status:`Unauthorized ~body:(`Assoc [("reason",`String "Permission denied")]) ()

let maybe_add_header k v headers =
	match v with
		| Some v -> Header.add headers k v
		| None -> headers

let handler ~document_root ~data_root ~user_db sock req body =
	let _serve_file fullpath =
		let file_ext = Some (snd (String.rsplit fullpath ".")) (* with String.Invalid_string -> None *) in
		let content_type = file_ext |> Option.map (function
			| ("html" | "css") as t -> "text/" ^ t
			| ("png" | "ico") as t -> "image/" ^ t
			| "js" -> "application/javascript"
			| "appcache" -> "text/plain"
			| "woff" -> "application/octet-stream"
			| ext -> log#warn "Unknown static file type: %s" ext; "application/octet-stream"
		) in
		let client_etag = Header.get (Cohttp.Request.headers req) "if-none-match" in
		lwt latest_etag =
			try_lwt
				Lwt_io.with_file ~mode:Lwt_io.input fullpath (fun f ->
					let hash = Sha256.init () in
					let chunks = Lwt_stream.from (fun () ->
						lwt str = Lwt_io.read ~count:1024 f in
						return (str |> Option.non_empty ~zero:"")
					) in
					lwt () = chunks |> Lwt_stream.iter (Sha256.update_string hash) in
					let digest = hash |> Sha256.finalize |> Sha256.to_bin |> Base64.encode in
					return (Some ("\"" ^ (digest ) ^ "\""))
				)
			with Unix.Unix_error (Unix.ENOENT, _, _) -> return_none
		in

		let headers = Header.init ()
			|> no_cache
			|> maybe_add_header content_type_key content_type in

		if match latest_etag, client_etag with
			| Some a, Some b -> a = b
			| _ -> false
		then
			Server.respond
				~body:Cohttp_lwt_body.empty
				~headers
				~status:`Not_modified ()
		else
			let headers = headers |> maybe_add_header "etag" latest_etag in
			Server.respond_file ~fname:fullpath ~headers () in

	let serve_static url = _serve_file (Server.resolve_file ~docroot:document_root ~uri:url) in
	let serve_file relpath = _serve_file (Filename.concat document_root relpath) in

	try_lwt
		let uri = Cohttp.Request.uri req in
		let data_path = Filename.concat data_root in
		let path = Uri.path uri in
		(* log#info "HIT: %s" path; *)
		let path = normpath path in

		let db_path_for user =
			data_path (Filename.concat "user_db" (user ^ ".json")) in
	
		let validate_token token = match token with
			| Some token ->
					let token = Auth.Token.of_json token in
					Auth.validate ~storage:user_db token
			| None -> return_none
		in

		let validate_user () =
			let tok = Header.get (Cohttp.Request.headers req) "Authorization" |> Option.bind (fun tok ->
				let tok =
					try Some (Str.split (Str.regexp " ") tok |> List.find (fun tok ->
							Str.string_match (Str.regexp "t=") tok 0
						))
					with Not_found -> None in
				tok |> Option.map (fun t -> String.sub t 2 ((String.length t) - 2) |> Uri.pct_decode |> J.from_string)
			) in
			validate_token tok
		in

		match Cohttp.Request.meth req with
			| `GET -> (
				match path with
					| ["db"] ->
							lwt user = validate_user () in
							begin match user with
								| Some user ->
									let username = user.Auth.User.name in
									log#debug "serving db for user: %s" username;
									Server.respond_file ~headers:json_content_type ~fname:(db_path_for username) ()
								| None -> respond_unauthorized ()
							end
					| [] -> serve_file "index.html"
					| ["hold"] -> Lwt.wait () |> Tuple.fst
					| _ -> serve_static uri
				)
			| `POST -> (
				lwt params = (
					lwt json = (Cohttp_lwt_body.to_string body) in
					log#debug "got body: %s" json;
					return (J.from_string json)
				) in

				let respond_token token =
					respond_json ~status:`OK ~body:(match token with
						| `Success tok -> `Assoc [("token", Auth.Token.to_json tok)]
						| `Failed msg -> `Assoc [("error", `String msg)]
					) ()
				in
				let mandatory = J.mandatory in

				match path with
					| ["auth"; "signup"] -> (
							let user = params |> mandatory J.string_field "user" in
							let password = params |> mandatory J.string_field "password" in
							lwt token = Auth.signup ~storage:user_db user password in
							respond_token token
					)
					| ["auth"; "login"] -> (
							let user = params |> mandatory J.string_field "user" in
							let password = params |> mandatory J.string_field "password" in
							lwt token = Auth.login ~storage:user_db user password in
							respond_token token
						)
					| ["auth"; "logout"] -> (
							let token = Auth.Token.of_json params in
							lwt () = Auth.logout ~storage:user_db token in
							respond_json ~status:`OK ~body:(`Assoc []) ()
						)
					| ["auth"; "validate"] -> (
						let token = Auth.Token.of_json params in
						lwt user = Auth.validate ~storage:user_db token in
						respond_json ~status:`OK ~body:(`Assoc [("valid",`Bool (Option.is_some user))]) ()
					)
					| ["db"] ->
							lwt user = validate_user () in
							begin match user with
								| None -> respond_unauthorized ()
								| Some user -> (
									let username = user.Auth.User.name in
									let db = params in
									let db_path = db_path_for username in
									log#debug "saving db for user: %s" username;
									(* XXX locking *)
									lwt db_file_contents = (try_lwt
										lwt contents = Lwt_io.with_file ~mode:Lwt_io.input db_path (fun f ->
											Lwt_stream.fold (^) (Lwt_io.read_lines f) ""
										) in
										return (Some contents)
										with Unix.Unix_error (Unix.ENOENT, _, _) -> return None
									) in

									let existing_core = db_file_contents |> Option.map J.from_string in
									let open Store in
									let open Store.Format in
									let existing_core = existing_core
										|> Option.map (fun json -> Store.Format.core.getter (Some json))
										|> Option.default empty_core in
									let changes = Some (db) |> Store.Format.changes.getter in
									let updated_core = {
										version = succ existing_core.version;
										records = Store.apply_changes existing_core changes;
									} |> Store.Format.core.setter |> Option.force in
									let tmp = (db_path ^ ".tmp") in
									lwt () = Lwt_io.with_file ~mode:Lwt_io.output tmp
										(fun f ->
											Lwt_io.write f (J.to_string ~std:true updated_core)
										)
									in
									lwt () = Lwt_unix.rename tmp db_path in
									respond_json ~status:`OK ~body:updated_core ()
								)
							end
					| _ -> Server.respond_not_found ~uri ()
				)
			| _ ->
				log#debug "unknown method; sending 500";
				Server.respond_error ~status:`Bad_request ~body:"unsupported method" ()
	with e ->
		let bt = Printexc.get_backtrace () in
		log#error "Error handling request: %s\n%s" (Printexc.to_string e) bt;
		raise e

let cwd = Unix.getcwd ()

let abs p = if Filename.is_relative p
	then Filename.concat cwd p
	else p

let start_server ~host ~port ~document_root ~data_root () =
	log#info "Listening on: %s %d" host port;
	let document_root = abs document_root
	and data_root = abs data_root in
	log#info "Document root: %s" document_root;
	log#info "Data root: %s" data_root;
	let user_db = new Auth.storage (Filename.concat data_root "users.db.json") in
	let conn_closed id () = log#info "connection %s closed"
			(Connection.to_string id) in
	let callback = handler ~document_root ~data_root ~user_db in
	let config = { Server.callback; conn_closed } in
	Server.create ~address:host ~port:port config

let main () =
	let open Extlib in
	let open OptParse in
	let open OptParser in

	let port = StdOpt.int_option ~default:8080 () in
	let host = StdOpt.str_option ~default:"127.0.0.1" () in
	let document_root = StdOpt.str_option ~default:"_build" () in
	let data_root = StdOpt.str_option ~default:"data" () in

	let options = OptParser.make ~usage: ("Usage: service [OPTIONS]") () in
	add options ~short_name:'p' ~long_name:"port" port;
	add options ~long_name:"host" host;
	add options ~long_name:"root" document_root;
	add options ~long_name:"data" data_root;
	let posargs = OptParse.OptParser.parse ~first:1 options Sys.argv in
	if List.length posargs <> 0 then (
		prerr_endline "Too many arguments";
		exit 1
	);
	let document_root = Opt.get document_root in
	let data_root = Opt.get data_root in
	let host = match (Opt.get host) with
		| "any" -> "0.0.0.0"
		| h -> h
	in
	Lwt_unix.run (start_server
		~port:(Opt.get port)
		~host
		~data_root
		~document_root
	())

