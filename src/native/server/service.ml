open Passe
open Common
open Lwt
module Header = Cohttp.Header
module Connection = Cohttp.Connection
module J = Json_ext


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


let string_of_method = function `GET -> "GET" | `POST -> "POST" | _ -> "[UNKNOWN METHOD]"

let maybe_add_header k v headers =
	match v with
		| Some v -> Header.add headers k v
		| None -> headers

module type SERVER = sig
	include Cohttp_lwt.Server
	(* these are conveniently provided by Cohttp_lwt_unix,
	 * we'll need shims for them on Mirage *)
	val resolve_file : docroot:string -> uri:Uri.t -> string
	val respond_file :
			?headers:Cohttp.Header.t ->
			fname:string -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
end

module Make (Logging:Logging.Sig) (Server: SERVER) (Fs: Filesystem.Sig) (Auth:Auth.Sig with module Fs = Fs) (Re:Re_ext.Sig) = struct
	module Store = Store.Make(Re)(Logging)
	let log = Logging.get_logger "service"

	let empty_user_db = (Store.empty_core |> Store.Format.json_of_core |> J.to_string)

	let user_db_dir data_root = Filename.concat data_root "user_db"
	let db_path_for data_root user = Filename.concat (user_db_dir data_root) (user ^ ".json")

		module User = Auth.User

	let respond_json ~status ~body () =
		Server.respond_string
			~headers:(json_content_type |> no_cache)
			~status ~body:(J.to_string body) ()

	let respond_error msg =
		respond_json ~status:`OK ~body:(`Assoc ["error",`String msg]) ()

	let respond_ok () = respond_json ~status:`OK ~body:(J.empty) ()

	let respond_unauthorized () =
		respond_json ~status:`Unauthorized ~body:(`Assoc [("reason",`String "Permission denied")]) ()

	let make_db fs data_root = new Auth.storage fs (Filename.concat data_root "users.db.json")

	let handler ~document_root ~data_root ~user_db ~fs ~enable_rc sock req body =
		(* hooks for unit test controlling *)
		let override_data_root = (fun newroot ->
			log#warn "setting data_root = %s" newroot;
			data_root := newroot;
			user_db := make_db fs newroot;
			let dbdir = Filename.dirname (db_path_for newroot "null") in
			match_lwt Fs.stat fs dbdir with
				| `Ok _ -> return_unit
				| `Error `No_directory_entry (_,_) -> begin
					Fs.unwrap_lwt "mkdir" (Fs.mkdir fs dbdir)
				end
				| `Error e -> Fs.fail "stat" e
		) in

		let data_root = !data_root and user_db = !user_db in
		let db_path_for = db_path_for data_root in

		let wipe_user_db = (fun username ->
			log#warn "wiping user DB for %s" username;
			let path = db_path_for username in
			match_lwt Fs.destroy fs path with
				| `Ok () | `Error `No_directory_entry (_,_) -> return_unit
				| `Error e -> Fs.fail "destroy" e
		) in

		let _serve_file fullpath =
			let file_ext = Some (snd (BatString.rsplit fullpath ".")) (* with String.Invalid_string -> None *) in
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
					let chunks = Fs.read_file_s fs fullpath in
					let hash = Sha256.init () in
					lwt () = chunks |> Lwt_stream.iter (Sha256.update_string hash) in
					let digest = hash |> Sha256.finalize |> Sha256.to_bin |> Base64.encode in
					return (Some ("\"" ^ (digest ) ^ "\""))
				with Fs.Error (Fs.ENOENT _) -> return_none
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
		let maybe_read_file path = try_lwt
				(* XXX streaming? *)
				lwt contents = Fs.read_file fs path in
				(* log#trace "read file contents: %s" contents; *)
				return (Some contents)
				with Fs.Error (Fs.ENOENT _) -> return_none
			in

		try_lwt
			let uri = Cohttp.Request.uri req in
			let path = Uri.path uri in
			log#debug "%s: %s" (string_of_method (Cohttp.Request.meth req)) path;
			let path = normpath path in

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

			let authorized fn =
				match_lwt validate_user () with
					| None -> respond_unauthorized ()
					| Some u -> fn u
			in

			let check_version () =
				match Header.get (Cohttp.Request.headers req) "x-passe-version" with
					| None -> log#debug "client did not provide a version - good luck!"
					| Some client_version ->
						(* this will be used when breaking format changes *)
						log#debug "Client version: %s" client_version;
						()
			in

			match Cohttp.Request.meth req with
				| `GET -> (
					match path with
						| ["db"] ->
								check_version ();
								authorized (fun user ->
									let username = User.name user in
									log#debug "serving db for user: %s" username;

									lwt body = maybe_read_file (db_path_for username) in
									let body = body |> Option.default_fn (fun () ->
										log#warn "no stored db found for %s" username;
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
								| (Some host, Some "http") when BatString.ends_with host ".rhcloud.com" ->
									let dest = Uri.with_scheme uri (Some "https") in
									Server.respond_redirect dest ()
								| _ ->
									serve_file "index.html"
							end
						| ["hold"] -> Lwt.wait () |> Tuple.fst
						| _ -> serve_static uri
					)
				| `POST -> (
					check_version ();
					lwt params = (
						lwt json = (Cohttp_lwt_body.to_string body) in
						(* log#trace "got body: %s" json; *)
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
						| "ctl" :: path when enable_rc -> begin
							match path with
							| ["init"] ->
									lwt () = params |> mandatory J.string_field "data" |> override_data_root in
									respond_ok ()
							| ["reset_db"] ->
									lwt () = params |> mandatory J.string_field "user" |> wipe_user_db in
									respond_ok ()
							| _ -> Server.respond_not_found ~uri ()
						end
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
						| ["auth"; "change-password"] -> (
								authorized (fun user ->
									let old = params |> J.mandatory J.string_field "old" in
									let new_password = params |> J.mandatory J.string_field "new" in
									lwt new_token = Auth.change_password ~storage:user_db user old new_password in
									match new_token with
										| Some tok ->
											respond_json ~status:`OK ~body:(tok|> Auth.Token.to_json) ()
										| None ->
											respond_error "Failed to update password"
								)
							)
						| ["auth"; "delete"] -> (
								authorized (fun user ->
									let username = User.name user in
									let password = params |> J.mandatory J.string_field "password" in
									(* delete user from DB, and also delete their DB *)
									lwt deleted = Auth.delete_user ~storage:user_db user password in
									if deleted then (
										log#warn "deleted user %s" username;
										lwt () = match_lwt Fs.destroy fs (db_path_for username) with
											| `Ok _ -> return_unit
											| `Error `No_directory_entry (_,_) -> return_unit
											| e -> Fs.unwrap "destroy" e |> return
										in
										respond_json ~status:`OK ~body:(J.empty) ()
									) else
										respond_error "Couldn't delete user (wrong password?)"
								)
							)
						| ["db"] ->
								authorized (fun user ->
									let username = User.name user in
									let db_path = db_path_for username in
									log#debug "saving db for user: %s" username;
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
											log#debug "not updating db; already at latest version";
											return core
										) else (
											let updated_core = {
												Store.apply_changes core changes with
												version = new_version;
											} in
											let tmp = (db_path ^ ".tmp") in
											let payload = updated_core |> json_of_core |> J.to_string in
											lwt () = Fs.unwrap_lwt "write" (Fs.write_file fs tmp payload) in
											lwt () = Fs.rename fs tmp db_path in
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
					log#debug "unknown method; sending 500";
					Server.respond_error ~status:`Bad_request ~body:"unsupported method" ()
		with e ->
			let bt = Printexc.get_backtrace () in
			log#error "Error handling request: %s\n%s" (Printexc.to_string e) bt;
			raise e

end
