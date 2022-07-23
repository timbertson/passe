open Passe
open Lwt
open Common
open Astring
module Header = Cohttp.Header
module Connection = Cohttp.Connection
module Body = Cohttp_lwt.Body
module Request = Cohttp.Request
module J = Json_ext
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

type file_extenstion = string
type file_response = [
	| `File of string list
	| `Dynamic of file_extenstion * string
]

let content_type_key = "Content-Type"
let content_type_header v = Header.init_with content_type_key v
let json_content_type = content_type_header "application/json"
let text_content_type = content_type_header "text/plain"
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

module Make
	(Version: Version.Sig)
	(Clock: Mirage_clock.PCLOCK)
	(Data: Dynamic_store.Sig)
	(Static_res:Static.Sig)
	(Server:Cohttp_lwt.S.Server)
	(Server_config:Server_config.Sig)
	(Auth:Auth.Sig with module Store = Data and module Clock = Clock)
	(Re:Re_ext.Sig)
= struct
	module Store = Store.Make(Re)
	module User = Auth.User

	module type AuthContext = sig
		val validate_user : Auth.storage -> Request.t
			-> (Auth.User.t option, Error.t) result Lwt.t

		val implicit_user : Request.t
			-> [`Anonymous | `Sandstorm_user of User.sandstorm_user] option

		val offline_access : bool
		val implicit_auth : bool
	end

	module SandstormAuth = struct
		let offline_access = false
		let implicit_auth = true
		let _validate_user req =
			let headers = Request.headers req in
			Header.get headers "X-Sandstorm-User-Id" |> Option.map (fun (id:string) ->
				let name: string = Header.get headers "X-Sandstorm-Username" |> Option.force |> Uri.pct_decode in
				`Sandstorm_user (User.sandstorm_user ~id ~name ())
			)

		let validate_user _db req = return (Ok (_validate_user req))
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
				| None -> return (Ok None)
			in

			let tok = Header.get (Request.headers req) "Authorization" |> Option.bind (fun tok ->
				let tok =
					try Some (Str.split (Str.regexp " ") tok |> List.find (fun tok ->
							Str.string_match (Str.regexp "t=") tok 0
						))
					with Not_found -> None in
				tok |> Option.map (fun t ->
					t |> String.drop ~max:2 |> Uri.pct_decode |> J.from_string
				)
			) in
			validate_token tok |> (Lwt_r.map % Option.map)
				(fun user -> `DB_user user)
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

	let db_path_for user =
		let components = ["user_db"; (string_of_uid user) ^ ".json"] in
		Path.make components

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

	let fallible_stream_of_results : ('a, 'e) result Lwt_stream.t -> 'a Lwt_stream.t
	= fun stream ->
		stream |> Lwt_stream.map (R.assert_ok Error.pp)

	type http_response = Cohttp.Response.t * Cohttp_lwt.Body.t

	let handler ~static ~data:initial_data ~enable_rc ~development =
		let module AuthContext = (val auth_context) in
		let offline_access = if development then false else AuthContext.offline_access in
		let user_db_path = Path.make ["users.db.json"] |> R.assert_ok Error.pp in

		let adopt_data_root root =
			let data : Data.t = root |> Option.fold (Data.reconnect initial_data) initial_data in
			(data, new Auth.storage data user_db_path)
		in

		let data, user_db =
			let (a,b) = adopt_data_root None in
			(ref a, ref b)
		in

		let response_of_result result =
			result |> Lwt.bindr (function
				| Ok response -> return response
				| Error e ->
					Server.respond_error
						~status:(`Code 500)
						~body:(Format.asprintf "internal error: %a" Error.pp e) ()
			)
		in

		(* hooks for unit test controlling *)
		let override_data_root = (fun newroot : unit ->
			Log.warn (fun m->m "setting data_root = %s" newroot);
			let new_data, new_user_db = adopt_data_root (Some newroot) in
			data := new_data;
			user_db := new_user_db
		) in

		let wipe_user_db = (fun uid ->
			Log.warn (fun m->m "wiping user DB for %s" (string_of_uid uid));
			let path = db_path_for uid |> R.assert_ok Error.pp in
			Data.delete !data path
		) in

		let respond_not_found =
			Server.respond_error ~status:`Not_found ~body:"not found" in

		let respond_file_error = function
			| `Invalid e -> Server.respond_error ~status:`Bad_request ~body:("invalid request: " ^ e) ()
			| `Failed e | `AssertionError e -> Server.respond_error ~status:(`Code 500) ~body:("internal error: " ^ e) ()
		in

		let maybe_read_file path =
			(* XXX streaming? *)
			Data.read !data path
		in

		let serve_file ~req ?headers contents = (
			let etag_buidler (type a): ((string -> unit) -> (unit -> string) -> a) -> a = fun apply ->
				let open Mirage_crypto.Hash in
				(* TODO: we could short-circuit allocations by iterating over cstructs directly *)
				let hash = ref SHA256.empty in
				let append chunk = hash := SHA256.feed !hash (Cstruct.of_string chunk) in
				let finalize (): string =
					let digest = !hash |> SHA256.get |> Cstruct.to_string |> Base64.encode |> Error.raise_result in
					("\"" ^ (digest ) ^ "\"")
				in
				apply append finalize
			in

			let etag_of_single chunk =
				etag_buidler (fun add_chunk complete ->
					add_chunk chunk;
					complete ()
				)
			in

			let etag_of_chunks chunks : (string, Error.t) result Lwt.t =
				etag_buidler (fun add_chunk complete ->
					let rec consume () =
						Lwt_stream.get chunks |> LwtMonad.bind (function
							| Some (Ok chunk) -> (
								add_chunk chunk;
								consume ()
							)
							| Some (Error _ as err) -> return err
							| None -> return (Ok (complete ()))
						)
					in
					consume ()
				)
			in

			let respond_file_chunks ~ext ~etag chunks : http_response Lwt.t = (
				let content_type = ext |> Option.map (function
					| ("html" | "css") as t -> "text/" ^ t
					| ("png" | "ico") as t -> "image/" ^ t
					| "js" -> "application/javascript"
					| "woff" -> "application/octet-stream"
					| ext ->
						Log.warn (fun m->m "Unknown static file type: %s" ext);
						"application/octet-stream"
				) in

				let client_etag = Header.get (Request.headers req) "if-none-match" in
				let headers = headers |> Option.default_fn Header.init
					|> no_cache
					|> maybe_add_header content_type_key content_type in

				if match etag, client_etag with
					| Some a, Some b -> a = b
					| _ -> false
				then
					Server.respond
						~body:Body.empty
						~headers
						~status:`Not_modified ()
				else (
					let headers = headers |> maybe_add_header "etag" etag in
					Server.respond
						~headers
						~status:`OK
						~body:(Body.of_stream chunks) ()
				)
			) in

			match contents with
				| `File path -> (
					return (Static_res.key path) |> Lwt_r.bind (fun key : (http_response option, Error.t) result Lwt.t ->
						let respond_chunks etag chunks : (http_response, Error.t) result Lwt.t =
							let rec last = function [] -> assert false | [x] -> x | _::tail -> last tail in
							let ext = String.cut ~rev:true ~sep:"." (last path) |> Option.map snd in
							respond_file_chunks ~ext ~etag:etag (fallible_stream_of_results chunks)
								|> Lwt.map (R.ok)
						in
						let static_etag : (string option, Error.t) result Lwt.t = Static_res.etag static key etag_of_chunks in
						static_etag |> Lwt_r.bind (function
							| Some _ as etag -> Static_res.read_s static key (respond_chunks etag)
							| None -> Lwt.return (Ok None)
						)
					) |> Lwt.bindr (function
						| Ok (Some response) -> return response
						| Ok None -> respond_not_found ()
						| Error e -> respond_file_error e
					)
				)
				| `Dynamic (ext, contents) -> (
					let etag = etag_of_single contents in
					respond_file_chunks ~ext:(Some ext) ~etag:(Some etag) (Lwt_stream.of_list [ contents ])
				)
		) in

	(* actual handle function *)
	(fun _sock req body ->
		let storage = !user_db in

		try%lwt
			let uri = Request.uri req in
			let path = Uri.path uri in
			Log.debug (fun m->m "+ %s: %s" (string_of_method (Request.meth req)) path);
			let path = normpath path in
			let validate_user () = AuthContext.validate_user !user_db req in
			let authorized fn =
				validate_user () |> Lwt_r.bind (function
					| None -> respond_unauthorized () |> ok_lwt
					| Some u -> fn u
				) |> response_of_result
			in
			let authorized_db fn =
				authorized (function
					| `DB_user u -> fn u
					| `Sandstorm_user _ -> respond_forbidden () |> Lwt.map R.ok
				)
			in

			let check_version () =
				match Header.get (Request.headers req) "x-passe-version" with
					| None -> Log.debug (fun m->m "client did not provide a version - good luck!")
					| Some client_version ->
						(* this will be used when breaking format changes *)
						Log.debug (fun m->m "Client version: %s" client_version);
						()
			in

			let response: http_response Lwt.t = (match Request.meth req with
				| `GET -> (
					match path with
						| ["db"] ->
								check_version ();
								authorized (fun user ->
									let uid = User.uid user in
									Log.debug (fun m->m "serving db for user: %s" (string_of_uid uid));
									db_path_for uid |> return
										|> Lwt_r.bind maybe_read_file
										|> Lwt_r.bindM (fun body ->
											let body = body |> Option.default_fn (fun () ->
												Log.warn (fun m->m "no stored db found for %s" (string_of_uid uid));
												empty_user_db
											) in
											Server.respond_string
												~headers:(json_content_type |> no_cache)
												~status:`OK ~body ()
										)
								)
						| [] ->
							let contents = Index.html
								~offline_access
								~implicit_auth:AuthContext.implicit_auth
								() |> Index.string_of_html in
							serve_file ~req
								~headers: (Header.init_with "X-UA-Compatible" "IE=Edge")
								(`Dynamic ("html", contents))

						| ["version"] ->
							Server.respond_string
								~headers:text_content_type
								~status:`OK ~body:(Version.pretty ()) ()

						| ["hold"] when development -> Lwt.wait () |> Tuple.fst
						| path ->
							serve_file ~req (`File path)
					)
				| `POST -> (
					check_version ();
					let _params = lazy (
						let%lwt json = (Body.to_string body) in
						(* Log.debug (fun m->m "got body: %s" json); *)
						return (J.from_string json)
					) in
					let params () = Lazy.force _params in

					let respond_token token =
						respond_json ~status:`OK ~body:(match token with
							| Ok tok -> `Assoc [("token", Auth.Token.to_json tok)]
							| Error msg -> `Assoc [("error", `String msg)]
						) ()
					in
					let respond_token_lwt token =
						token |> Lwt.bindr respond_token
					in
					let mandatory = J.mandatory in

					match path with
						| "ctl" :: path when enable_rc -> begin
							match path with
							| ["init"] ->
									let%lwt params = params () in
									(params
										|> mandatory J.string_field "data"
										|> override_data_root
									);
									respond_ok ()
							| ["reset_db"] ->
									let%lwt params = params () in
									params
										|> mandatory J.string_field "user"
										|> Auth.User.uid_of_string
										|> wipe_user_db
										|> Lwt_r.bindM respond_ok
										|> response_of_result
							| _ -> Server.respond_not_found ~uri ()
						end
						| ["auth"; "signup"] -> (
								let%lwt params = params () in
								let user = params |> mandatory J.string_field "user" in
								let password = params |> mandatory J.string_field "password" in
								Auth.signup ~storage user password |> respond_token_lwt
						)
						| ["auth"; "login"] -> (
								let%lwt params = params () in
								let user = params |> mandatory J.string_field "user" in
								let password = params |> mandatory J.string_field "password" in
								Auth.login ~storage user password |> respond_token_lwt
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
								let%lwt params = params () in
								let token = Auth.Token.of_json params in
								Auth.logout ~storage token |> Lwt_r.bindM (fun () ->
									respond_json ~status:`OK ~body:(`Assoc []) ()
								) |> response_of_result
							)
						| ["auth"; "validate"] -> (
								let%lwt params = params () in
								let token = Auth.Token.of_json params in
								Auth.validate ~storage token |> Lwt_r.bindM (fun user ->
									respond_json ~status:`OK ~body:(`Assoc [("valid",`Bool (Option.is_some user))]) ()
								) |> response_of_result
						)
						| ["auth"; "change-password"] -> (
								let%lwt params = params () in
								authorized_db (fun user ->
									let old = params |> J.mandatory J.string_field "old" in
									let new_password = params |> J.mandatory J.string_field "new" in
									Auth.change_password ~storage user old new_password |> Lwt_r.bindM (function
										| Some tok ->
											respond_token (Ok tok)
										| None ->
											respond_error "Failed to update password"
									)
								)
							)
						| ["auth"; "delete"] -> (
								let%lwt params = params () in
								authorized_db (fun user ->
									let uid = User.uid_db user in
									let password = params |> J.mandatory J.string_field "password" in
									db_path_for uid |> return |> Lwt_r.bind (fun db_path ->
										(* delete user from DB, and also delete their DB *)
										Auth.delete_user ~storage user password |> Lwt_r.bind (fun deleted ->
											if deleted then (
												Log.warn (fun m->m "deleted user %s" (User.string_of_uid uid));
												Data.delete !data db_path |> Lwt_r.bindM (fun () ->
													respond_json ~status:`OK ~body:(J.empty) ()
												)
											) else respond_error "Couldn't delete user (wrong password?)" |> ok_lwt
										)
									)
								)
							)
						| ["db"] ->
								let%lwt params = params () in
								let process_db_changes ~db_path db_file_contents = (
									let submitted_changes = params |> J.mandatory J.get_field "changes" in
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
										let core = if new_version = stored_core.version then (
											Log.debug (fun m->m "not updating db; already at latest version");
											return (Ok core)
										) else (
											let updated_core = {
												(Store.apply_changes core changes) with
												version = new_version;
											} in
											let payload = updated_core |> json_of_core |> J.to_string in
											Data.write !data db_path payload |> Lwt_r.map (fun () ->
												updated_core
											)
										) in
										core |> Lwt_r.bindM (fun core ->
											respond_json ~status:`OK ~body:(
												if client_version = core.version
												then
													(* client has the latest DB, and no changes were made.
														* Just respond with the version. *)
													build_assoc [ store_field version core.version ]
												else
													json_of_core core
											) ()
										)
									in

									let existing_version = stored_core.version in
									if existing_version < client_version then (
										match submitted_core with
											| None ->
												(* Uh oh! the client has a newer version than us. Request a full update *)
												respond_json ~status:`Conflict ~body:(`Assoc ["stored_version", `Int existing_version]) ()
													|> ok_lwt
											| Some core ->
												(* client sent us the full DB, so just use it *)
												core |> Store.Format.core_of_json |> process
									) else (
										process stored_core
									)
								) in

								authorized (fun user ->
									let uid = User.uid user in
									Log.debug (fun m->m "saving db for user: %s" (string_of_uid uid));
									db_path_for uid |> return |> Lwt_r.bind (fun db_path ->
										(* XXX locking *)
										maybe_read_file db_path
											|> Lwt_r.bind (process_db_changes ~db_path)
									)
								)
						| _ -> Server.respond_not_found ~uri ()
					)
				| _ ->
					Log.debug (fun m->m "unknown method; sending 500");
					Server.respond_error ~status:`Bad_request ~body:"unsupported method" ()
			) in
			response
		with e ->
			let bt = Printexc.get_backtrace () in
			Log.err (fun m->m "Error handling request: %s\n%s" (Printexc.to_string e) bt);
			raise e
	)

end
