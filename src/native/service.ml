open Lwt
open Cohttp
open Cohttp_lwt_unix
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

let respond_json ~status ~body () =
	Server.respond_string
		~headers:(Header.init_with "Content-Type" "application/json")
		~status ~body:(J.to_string ~std:true body) ()

let handler ~document_root ~data_root sock req body =
	let uri = Cohttp.Request.uri req in
	let data_path = Filename.concat data_root in
	let document_path = Filename.concat document_root in
	let path = Uri.path uri in
	log#info "HIT: %s" path;
	let path = normpath path in
	let serve_file relpath =
		Server.respond_file ~fname:(document_path relpath) () in
	let db_path_for user =
		data_path (Filename.concat "user_db" (user ^ ".json")) in
	let json_body = lazy (
		lwt json = (Cohttp_lwt_body.to_string body) in
		return (J.from_string json)
	) in

	match Cohttp.Request.meth req with
		| `GET -> (
			match path with
				| ["register"] -> Server.respond_string ~status:`OK ~body:"reg!" ()
				| ["db"; user] ->
						(* XXX authentication! *)
						log#debug "serving db for user: %s" user;
						Server.respond_file ~fname:(db_path_for user) ()
				| [] -> serve_file "index.html"
				| _ -> serve_file (String.concat "/" path)
			)
		| `POST -> (
			match path with
				| ["auth"; "login"] -> (
						lwt body = (Cohttp_lwt_body.to_string body) in
						log#debug "got body: %s" body;
						let params = J.from_string body in
						let user = params |> J.get_field "user" |> Option.bind J.as_string in
						let password = params |> J.get_field "password" |> Option.bind J.as_string in
						match (user, password) with
							(* XXX authentication *)
							| (Some user, Some password) when user = password ->
									respond_json
										~status:`OK
										~body:(`Assoc [("user",`String user); ("token",`String "letmein")])
										()
							| _ -> respond_json ~status:`Unauthorized ~body:(`Assoc [("error",`String "Incorrect username or password")]) ()
					)
				| ["auth"; "validate"] -> (
					lwt body = Lazy.force json_body in
					lwt () = Lwt_unix.sleep 1.0 in
					(* respond_json ~status:`OK ~body:(`Null) () *)
					respond_json ~status:`Unauthorized ~body:(`Assoc [("reason", `String "nope")]) ()
				)
				| ["db"; user] ->
						(* XXX authentication *)
						log#debug "saving db for user: %s" user;
						(* XXX locking *)
						let existing_core = try
							Some (J.from_file (db_path_for user))
							with Unix.Unix_error (Unix.ENOENT, _, _) -> None
						in
						let open Store in
						let open Store.Format in
						let existing_core = existing_core
							|> Option.map (fun json -> Store.Format.core.getter (Some json))
							|> Option.default empty_core in
						lwt body = body |> Cohttp_lwt_body.to_string in
						let changes = Some (J.from_string body) |> Store.Format.changes.getter in
						let updated_core = {
							version = succ existing_core.version;
							records = Store.apply_changes existing_core changes;
						} |> Store.Format.core.setter |> Option.force in
						let tmp = ((db_path_for user) ^ ".tmp") in
						lwt () = Lwt_io.with_file
							~mode:Lwt_io.output
							tmp
							(fun f ->
								Lwt_io.write f (J.to_string ~std:true updated_core)
							)
						in
						lwt () = Lwt_unix.rename tmp (db_path_for user) in
						respond_json ~status:`OK ~body:updated_core ()
				| _ -> Server.respond_not_found ~uri ()
			)
		| _ ->
			log#debug "unknown method; sending 500";
			Server.respond_error ~status:`Bad_request ~body:"unsupported method" ()

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
	let conn_closed id () = log#info "connection %s closed"
			(Connection.to_string id) in
	let callback = handler ~document_root ~data_root in
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
	Lwt_unix.run (start_server
		~port:(Opt.get port)
		~host:(Opt.get host)
		~document_root:(Opt.get document_root)
		~data_root:(Opt.get data_root)
	())

let () = main ()
