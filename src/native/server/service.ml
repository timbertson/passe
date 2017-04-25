open Passe
open Lwt
open Common
open Astring
module Header = Cohttp.Header
module Connection = Cohttp.Connection
module J = Json_ext
module Path = FilePath.UnixPath.Abstract
module Str = Re_str

let slash = Str.regexp "/"

let rec any pred lst = match lst with
	| [] -> false
	| hd::tail -> if pred hd then true else any pred tail

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

type file_response = [
	| `File of string
	| `Dynamic of string * string
]

let content_type_key = "Content-Type"
let content_type_header v = Header.init_with content_type_key v
let json_content_type = content_type_header "application/json"
let no_cache h = Header.add h "Cache-control" "no-cache"


let string_of_method = function
	| `GET -> "GET"
	| `POST -> "POST"
	| `PUT -> "PUT"
	| `DELETE -> "DELETE"
	| `PATCH -> "PATCH"
	| `HEAD -> "HEAD"
	| `TRACE -> "TRACE"
	| `OPTIONS -> "OPTIONS"
	| `CONNECT -> "CONNECT"
	| `Other s -> s

let maybe_add_header k v headers =
	match v with
		| Some v -> Header.add headers k v
		| None -> headers

module Make (Clock: Mirage_types.PCLOCK) (Static_res:Static.Sig) (Fs: Filesystem.Sig) (Server:Cohttp_lwt.Server) (Server_config:Server_config.Sig) (Auth:Auth.Sig with module Fs = Fs and module Clock = Clock) (Re:Re_ext.Sig) = struct
	module Store = Store.Make(Re)
	module User = Auth.User
	module Data_res = Static.Fs(Fs)

	module type AuthContext = sig
		val validate_user : Auth.storage -> Cohttp.Request.t -> Auth.User.t option Lwt.t
		val implicit_user : Cohttp.Request.t -> [`Anonymous | `Sandstorm_user of User.sandstorm_user] option
		val offline_access : bool
		val implicit_auth : bool
	end

	module SandstormAuth = struct
		let offline_access = false
		let implicit_auth = true
		let _validate_user req =
			let headers = Cohttp.Request.headers req in
			Header.get headers "X-Sandstorm-User-Id" |> Option.map (fun (id:string) ->
				let name: string = Header.get headers "X-Sandstorm-Username" |> Option.force |> Uri.pct_decode in
				`Sandstorm_user (User.sandstorm_user ~id ~name ())
			)

		let validate_user _db req = return (_validate_user req)
		let implicit_user req = Some (match _validate_user req with
			| Some user -> user
			| None -> `Anonymous
		)
	end

	module StandaloneAuth = struct
		let offline_access = true
		let implicit_auth = false
		let implicit_user _req = None
		let validate_user user_db = fun req ->
			let validate_token token = match token with
				| Some token ->
						let token = Auth.Token.of_json token in
						Auth.validate ~storage:user_db token
				| None -> return_none
			in

			let tok = Header.get (Cohttp.Request.headers req) "Authorization" |> Option.bind (fun tok ->
				let tok =
					try Some (Str.split (Str.regexp " ") tok |> List.find (fun tok ->
							Str.string_match (Str.regexp "t=") tok 0
						))
					with Not_found -> None in
				tok |> Option.map (fun t ->
					t |> String.drop ~max:2 |> Uri.pct_decode |> J.from_string
				)
			) in
			lwt user = validate_token tok in
			return (user |> Option.map (fun user -> `DB_user user))
	end

	module Log = (val Logging.log_module "service")
	let auth_context : (module AuthContext) =
		if (Server_config.is_sandstorm ()) then (
			Log.info (fun m->m "SANDSTORM=1; using sandstorm auth mode");
			(module SandstormAuth)
		) else (
			(module StandaloneAuth)
		)

	let string_of_uid = Auth.User.string_of_uid

	let empty_user_db = (Store.empty_core |> Store.Format.json_of_core |> J.to_string)

	let user_db_dir data_root = Filename.concat data_root "user_db"
	let db_path_for data_root uid = Filename.concat
		(user_db_dir data_root)
		((string_of_uid uid) ^ ".json")

	let respond_json ~status ~body () =
		Server.respond_string
			~headers:(json_content_type |> no_cache)
			~status ~body:(J.to_string body) ()

	let respond_error msg =
		respond_json ~status:`OK ~body:(`Assoc ["error",`String msg]) ()

	let respond_ok () = respond_json ~status:`OK ~body:(J.empty) ()

	let respond_unauthorized () =
		respond_json ~status:`Unauthorized ~body:(`Assoc [("reason",`String "Permission denied")]) ()

	let respond_forbidden () =
		respond_json ~status:`Forbidden ~body:(`Assoc [("reason",`String "Request forbidden")]) ()

	let handler ~static ~data_root:initial_data_root ~clock ~fs ~enable_rc ~development = fun _sock req body ->
		let module AuthContext = (val auth_context) in
		let offline_access = if development then false else AuthContext.offline_access in

		let adopt_data_root root = (
			root,
			Data_res.init ~fs root,
			new Auth.storage clock fs (Filename.concat root "users.db.json")
		) in

		let data_root, data_ro, user_db =
			let (a,b,c) = adopt_data_root initial_data_root in
			(ref a, ref b, ref c)
		in

		(* hooks for unit test controlling *)
		let override_data_root = (fun newroot ->
			Log.warn (fun m->m "setting data_root = %s" newroot);
			let _root, _data_ro, _user_db = adopt_data_root newroot in
			data_root := newroot;
			data_ro := _data_ro;
			user_db := _user_db;

			let dbdir = Filename.dirname (db_path_for newroot (Auth.User.uid_of_string "null")) in
			match_lwt Fs.stat fs dbdir with
				| Ok _ -> return_unit
				| Error `No_directory_entry -> begin
					Fs.unwrap_write_lwt "mkdir" (Fs.mkdir fs dbdir)
				end
				| Error e -> Fs.fail "stat" e
		) in

		let db_path_for uid = db_path_for !data_root uid in

		let wipe_user_db = (fun uid ->
			Log.warn (fun m->m "wiping user DB for %s" (string_of_uid uid));
			let path = db_path_for uid in
			Fs.destroy_if_exists fs path |> Fs.unwrap_write_lwt "destroy"
		) in

		let respond_file_error = function
			| `Not_found -> Server.respond_error ~status:`Not_found ~body:"not found" ()
			| `Invalid_path -> Server.respond_error ~status:`Bad_request ~body:"invalid path" ()
			| `Read_error e -> Server.respond_error ~status:(`Code 500) ~body:("internal error: " ^ e) ()
		in

		let serve_file ?headers contents = (
			let etag_of_chunks chunks =
				let open Nocrypto.Hash in
				(* TODO: we could short-circuit allocations by iterating over cstructs directly *)
				let hash = SHA256.init () in
				lwt () = Lwt_stream.iter (fun chunk ->
					SHA256.feed hash (Cstruct.of_string chunk)
				) chunks in
				let digest = hash |> SHA256.get |> Cstruct.to_string |> Base64.encode in
				return ("\"" ^ (digest ) ^ "\"")
			in

			let respond_file_chunks ~ext ~etag iter_file_chunks = (
				let content_type = ext |> Option.map (function
					| ("html" | "css") as t -> "text/" ^ t
					| ("png" | "ico") as t -> "image/" ^ t
					| "js" -> "application/javascript"
					| "appcache" -> "text/plain"
					| "woff" -> "application/octet-stream"
					| ext ->
						Log.warn (fun m->m "Unknown static file type: %s" ext);
						"application/octet-stream"
				) in

				let client_etag = Header.get (Cohttp.Request.headers req) "if-none-match" in
				let headers = headers |> Option.default_fn Header.init
					|> no_cache
					|> maybe_add_header content_type_key content_type in

				if match etag, client_etag with
					| Some a, Some b -> a = b
					| _ -> false
				then
					Server.respond
						~body:Cohttp_lwt_body.empty
						~headers
						~status:`Not_modified ()
				else (
					let headers = headers |> maybe_add_header "etag" etag in
					lwt response = iter_file_chunks (fun contents ->
						Server.respond
							~headers
							~status:`OK
							~body:(Cohttp_lwt_body.of_stream contents) ()
					) in
					match response with
						| Ok result -> return result
						| Error e -> respond_file_error e
				)
			) in

			match contents with
				| `File path -> (
					lwt etag = Static_res.etag static path etag_of_chunks in
					lwt etag_and_read = (match etag with
						| Ok etag -> return (Ok (etag, Static_res.read_s static path))
						| Error `Invalid_path | Error `Read_error _ as e -> return e
						| Error `Not_found -> (
							(* try data *)
							Log.debug (fun m->m "Path %s not found in static files; trying data" path);
							let data_ro = !data_ro in
							Data_res.etag data_ro path etag_of_chunks |> Lwt_r.map (
								fun etag -> (etag, Data_res.read_s data_ro path)
							)
						)
					) in
					match etag_and_read with
						| Ok (etag, read) -> (
							let ext = String.cut ~rev:true ~sep:"." path |> Option.map snd in
							respond_file_chunks ~ext ~etag:(Some etag) read
						)
						| Error e -> respond_file_error e
				)
				| `Dynamic (ext, contents) ->
					let contents = (Lwt_stream.of_list [ contents ]) in
					lwt etag = etag_of_chunks contents in
					let iter_file_chunks fn = fn contents |> Lwt.map R.ok in
					respond_file_chunks ~ext:(Some ext) ~etag:(Some etag) iter_file_chunks
		) in

		let maybe_read_file path = try_lwt
				(* XXX streaming? *)
				lwt contents = Fs.read_file fs path in
				(* Log.debug (fun m->m "read file contents: %s" contents); *)
				return (Some contents)
			with Fs.FsError Fs.ENOENT -> return_none
		in

		try_lwt
			let uri = Cohttp.Request.uri req in
			let path = Uri.path uri in
			Log.debug (fun m->m "+ %s: %s" (string_of_method (Cohttp.Request.meth req)) path);
			let path = normpath path in
			let validate_user () = AuthContext.validate_user !user_db req in
			let authorized fn =
				match_lwt validate_user () with
					| None -> respond_unauthorized ()
					| Some u -> fn u
			in
			let authorized_db fn =
				authorized (function
					| `DB_user u -> fn u
					| `Sandstorm_user _ -> respond_forbidden ()
				)
			in

			let check_version () =
				match Header.get (Cohttp.Request.headers req) "x-passe-version" with
					| None -> Log.debug (fun m->m "client did not provide a version - good luck!")
					| Some client_version ->
						(* this will be used when breaking format changes *)
						Log.debug (fun m->m "Client version: %s" client_version);
						()
			in

			match Cohttp.Request.meth req with
				| `GET -> (
					match path with
						| ["db"] ->
								check_version ();
								authorized (fun user ->
									let uid = User.uid user in
									Log.debug (fun m->m "serving db for user: %s" (string_of_uid uid));

									lwt body = maybe_read_file (db_path_for uid) in
									let body = body |> Option.default_fn (fun () ->
										Log.warn (fun m->m "no stored db found for %s" (string_of_uid uid));
										empty_user_db
									) in

									Server.respond_string
										~headers:(json_content_type |> no_cache)
										~status:`OK ~body ()
								)
						| [] ->
							let h = (Cohttp.Request.headers req) in
							(* redirect http -> https on openshift *)
							begin match (Header.get h "host", Header.get h "x-forwarded-proto") with
								| (Some host, Some "http") when String.is_suffix ~affix:".rhcloud.com" host ->
									let dest = Uri.with_scheme uri (Some "https") in
									Server.respond_redirect dest ()
								| _ ->
									let contents = Index.html
										~offline_access
										~implicit_auth:AuthContext.implicit_auth
										() |> Index.string_of_html in
									serve_file
										~headers: (Header.init_with "X-UA-Compatible" "IE=Edge")
										(`Dynamic ("html", contents))
							end

						| ["index.appcache"] when not offline_access ->
							Server.respond_error ~status:`Not_found ~body:"not found" ()

						| ["hold"] when development -> Lwt.wait () |> Tuple.fst
						| path ->
							Static.key_of_path_components path |> R.map (fun path ->
								serve_file (`File path)
							) |> R.recover respond_file_error
					)
				| `POST -> (
					check_version ();
					let _params = lazy (
						lwt json = (Cohttp_lwt_body.to_string body) in
						(* Log.debug (fun m->m "got body: %s" json); *)
						return (J.from_string json)
					) in
					let params () = Lazy.force _params in

					let respond_token token =
						respond_json ~status:`OK ~body:(match token with
							| `Success tok -> `Assoc [("token", Auth.Token.to_json tok)]
							| `Failed msg -> `Assoc [("error", `String msg)]
						) ()
					in
					let mandatory = J.mandatory in

					match path with
						| "ctl" :: path when enable_rc -> begin
							match path with
							| ["init"] ->
									lwt params = params () in
									lwt () = params |> mandatory J.string_field "data" |> override_data_root in
									respond_ok ()
							| ["reset_db"] ->
									lwt params = params () in
									lwt () = params |> mandatory J.string_field "user" |> Auth.User.uid_of_string |> wipe_user_db in
									respond_ok ()
							| _ -> Server.respond_not_found ~uri ()
						end
						| ["auth"; "signup"] -> (
								lwt params = params () in
								let user = params |> mandatory J.string_field "user" in
								let password = params |> mandatory J.string_field "password" in
								lwt token = Auth.signup ~storage:!user_db user password in
								respond_token token
						)
						| ["auth"; "login"] -> (
								lwt params = params () in
								let user = params |> mandatory J.string_field "user" in
								let password = params |> mandatory J.string_field "password" in
								lwt token = Auth.login ~storage:!user_db user password in
								respond_token token
							)
						| ["auth"; "state"] -> (
								match AuthContext.implicit_user req with
									| None ->
										Log.debug (fun m->m "auth/state requested, but there is no implicit user state");
										Server.respond_not_found ~uri ()
									| Some auth ->
										let response = (match auth with
											| `Anonymous -> `Assoc []
											| `Sandstorm_user u -> User.json_of_sandstorm u
										) in
										respond_json ~status:`OK ~body:response ()
							)
						| ["auth"; "logout"] -> (
								lwt params = params () in
								let token = Auth.Token.of_json params in
								lwt () = Auth.logout ~storage:!user_db token in
								respond_json ~status:`OK ~body:(`Assoc []) ()
							)
						| ["auth"; "validate"] -> (
								lwt params = params () in
								let token = Auth.Token.of_json params in
								lwt user = Auth.validate ~storage:!user_db token in
								respond_json ~status:`OK ~body:(`Assoc [("valid",`Bool (Option.is_some user))]) ()
						)
						| ["auth"; "change-password"] -> (
								lwt params = params () in
								authorized_db (fun user ->
									let old = params |> J.mandatory J.string_field "old" in
									let new_password = params |> J.mandatory J.string_field "new" in
									lwt new_token = Auth.change_password ~storage:!user_db user old new_password in
									match new_token with
										| Some tok ->
											respond_token (`Success tok)
										| None ->
											respond_error "Failed to update password"
								)
							)
						| ["auth"; "delete"] -> (
								lwt params = params () in
								authorized_db (fun user ->
									let uid = User.uid_db user in
									let password = params |> J.mandatory J.string_field "password" in
									(* delete user from DB, and also delete their DB *)
									lwt deleted = Auth.delete_user ~storage:!user_db user password in
									if deleted then (
										Log.warn (fun m->m "deleted user %s" (User.string_of_uid uid));
										lwt () =
											Fs.destroy_if_exists fs (db_path_for uid)
											|> Fs.unwrap_write_lwt "destroy" in
										respond_json ~status:`OK ~body:(J.empty) ()
									) else
										respond_error "Couldn't delete user (wrong password?)"
								)
							)
						| ["db"] ->
								lwt params = params () in
								authorized (fun user ->
									let uid = User.uid user in
									Log.debug (fun m->m "saving db for user: %s" (string_of_uid uid));
									let db_path = db_path_for uid in
									(* XXX locking *)
									let submitted_changes = params |> J.mandatory J.get_field "changes" in
									lwt db_file_contents = maybe_read_file db_path in

									(* either the client sends {changes, version} or {changes, core={version}} *)
									let submitted_core = params |> J.get_field "core" in
									let client_version = submitted_core |> Option.default params |> J.mandatory J.int_field "version" in

									let open Store in
									let open Store.Format in
									let stored_core = db_file_contents |> Option.map J.from_string
										|> Option.map core_of_json
										|> Option.default empty_core in

									let process core =
										let changes = submitted_changes |> changes_of_json in
										(* version doesn't increment when change list is empty *)
										let new_version = if changes = [] then core.version else succ core.version in
										(* note that stored_core.version may be < core.version even when there are no changes,
											* if the client submitted a core db that's newer than ours *)
										lwt core = if new_version = stored_core.version then (
											Log.debug (fun m->m "not updating db; already at latest version");
											return core
										) else (
											let updated_core = {
												Store.apply_changes core changes with
												version = new_version;
											} in
											let payload = updated_core |> json_of_core |> J.to_string in
											lwt () = Fs.write_file fs db_path payload in
											return updated_core
										) in
										respond_json ~status:`OK ~body:(
											if client_version = core.version
											then
												(* client has the latest DB, and no changes were made.
													* Just respond with the version. *)
												build_assoc [ store_field version core.version ]
											else
												json_of_core core
										) ()
									in

									let existing_version = stored_core.version in
									if existing_version < client_version then (
										match submitted_core with
											| None ->
												(* Uh oh! the client has a newer version than us. Request a full update *)
												respond_json ~status:`Conflict ~body:(`Assoc ["stored_version", `Int existing_version]) ()
											| Some core ->
												(* client sent us the full DB, so just use it *)
												core |> Store.Format.core_of_json |> process
									) else (
										process stored_core
									)
								)
						| _ -> Server.respond_not_found ~uri ()
					)
				| _ ->
					Log.debug (fun m->m "unknown method; sending 500");
					Server.respond_error ~status:`Bad_request ~body:"unsupported method" ()
		with e ->
			let bt = Printexc.get_backtrace () in
			Log.err (fun m->m "Error handling request: %s\n%s" (Printexc.to_string e) bt);
			raise e

end
