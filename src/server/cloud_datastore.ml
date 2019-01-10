open Cohttp
open Cohttp_lwt
module Response = Cohttp.Response
open Passe
open Common
module J = Json_ext
module Log = (val Passe.Logging.log_module "cloud_datastore")

type Dynamic_store.connector += Cloud_datastore of string

type request_error = [
	| Error.t
	| `Response of Response.t * Body.t
]

(* split OK / Error responses into internal error type, which keeps `response` object *)
let result_of_response (response, body) =
	let status = Response.status response in
	Log.debug (fun m->m"HTTP response: %a" Response.pp_hum response);
	if status |> Code.code_of_status |> Code.is_success
		then Ok (response, body)
		else Error (`Response (response, body))

let consume_and_log body =
	(* Note: we always want to consume the body, as that eagerly cleans up resources *)
	Body.to_string body |> Lwt.map (fun body ->
		Log.debug (fun m->m "response body: %s" body)
	)

let consume_and_log_body (_response, body) = consume_and_log body

let json_field name = function
	| `Assoc pairs -> List.assoc_opt name pairs
	| _ -> None

let json_string = function `String x -> Some x | _ -> None

let json_singleton = function
	| `List [el] -> Some el
	| _ -> None

let expected_json_field json field =
	(* Note: we only debug log the contents, including it in the error would leak secrets *)
	let msg = "expected JSON object with " ^ field in
	Log.debug (fun m->m"%s: %s" msg (J.to_string json));
	`Failed msg

(* promote `Not_found to Ok None *)
let allowing_404 = function
	| Ok x -> Lwt.return (Ok (Some x))
	| Error (`Response (response, body)) when (Response.status response) = `Not_found ->
		consume_and_log body |> Lwt.map (fun () -> Ok None)
	| Error _ as err -> Lwt.return err

(* turn (_, request_error) result into (_, Error.t) result *)
let to_standard_error : ('a, request_error) result -> ('a, Error.t) result Lwt.t = function
	| Ok x -> Lwt.return (Ok x)
	| Error (`Response (response, body)) ->
		let status = Response.status response in
		consume_and_log body |> Lwt.map (fun () ->
			Error (`Failed ("HTTP response: " ^ (Code.string_of_status status)))
		)
	| Error #Error.t as e -> Lwt.return (e)

let kind = "doc"

let append_path p u = Uri.with_path u (Uri.path u ^ p)

let key_of_path path = `Assoc ["path", `List [
	`Assoc ["name", `String (Path.to_string path); "kind", `String kind]
]]

let parse_json s = try Ok (J.from_string s) with e -> Error (`Failed (Printexc.to_string e))

module Keys = struct
	let access_token = "access_token"
	let assertion = "assertion"
	let content = "content"
	let delete = "delete"
	let entity = "entity"
	let excludeFromIndexes = "excludeFromIndexes"
	let found = "found"
	let grant_type = "grant_type"
	let key = "key"
	let missing = "missing"
	let properties = "properties"
	let stringValue = "stringValue"
	let upsert = "upsert"
end

module Make (Client:Cohttp_lwt.S.Client) = struct
	module Impl = (struct (* ( ) *)
		include Dynamic_store.Types
		type t = {
			url : Uri.t;
			account : string;
			token_uri : string;
			private_key : Nocrypto.Rsa.priv;
			token : string option ref;
			ctx : Client.ctx;
		}

		module PathLock = Lock.Map(Path.Relative)()

		let _post ~ctx ~bearer ~body url : ((Response.t * Body.t), request_error) result Lwt.t =
			let headers = let base = Header.init () in
				match bearer with
					| Some b -> Header.add base "Authorization" ("Bearer " ^ b)
					| None -> base
			in
			Log.debug (fun m->m"Posting (%s) to %a with body %s"
				(match bearer with Some _ -> "authenticated" | None -> "unauthenticated") Uri.pp_hum url body);

			try%lwt
				Client.post ~ctx ~headers ~body:(Body.of_string body) url
					|> Lwt.map result_of_response
			with e -> Lwt.return (
				let msg = Format.asprintf "exception thrown in POST to %a" Uri.pp_hum url in
				Log.debug (fun m->m"%s: %s" msg (Printexc.to_string e));
				Error (`Failed msg)
			)

		let get_access_token t =
			(* see https://developers.google.com/identity/protocols/OAuth2ServiceAccount *)
			let jwt =
				let header = `Assoc [
					"alg", `String "RS256";
					"typ", `String "JWT";
				] |> J.to_string in
				let now = Unix.time () |> int_of_float in
				let claim = `Assoc [
					"iss", `String t.account;
					"scope", `String "https://www.googleapis.com/auth/datastore";
					"aud", `String t.token_uri;
					"exp", `Int (now + (60 * 50)); (* 50 minutes *)
					"iat", `Int now;
				] |> J.to_string in

				(* from https://hackernoon.com/rs256-in-ocaml-reasonml-9ae579b9420a *)
				let rs256_sign data =
					let data = Cstruct.of_string data in
					let h = Nocrypto.Hash.SHA256.digest data in
					let pkcs1_digest = X509.Encoding.pkcs1_digest_info_to_cstruct (`SHA256, h) in
					Nocrypto.Rsa.PKCS1.sig_encode ~key:t.private_key pkcs1_digest |> Cstruct.to_string
				in

				Log.debug (fun m->m "generated JWT claim: %s.%s" header claim);
				let serialized = (Base64.encode header) ^ "." ^ (Base64.encode claim) in
				let signature = rs256_sign serialized |> Base64.encode in
				serialized ^ "." ^ signature
			in

			let body = `Assoc Keys.[
				grant_type, `String "urn:ietf:params:oauth:grant-type:jwt-bearer";
				assertion, `String jwt;
			] |> J.to_string in
			_post ~ctx:t.ctx ~bearer:None ~body (Uri.of_string t.token_uri)
				|> Lwt_r.bind (fun (_, body) ->
					Body.to_string body |> Lwt.map (fun body ->
						parse_json body
							|> R.bindr (fun json ->
								json |> json_field Keys.access_token |> (function
									| Some (`String token) -> Ok token
									| _ -> Error (expected_json_field json "access_token")
								)
							)
					)
				)

		let post instance ~body url =
			let _post token = _post ~ctx:instance.ctx ~bearer:(Some token) ~body url in

			let refresh_token () =
				instance.token := None;
				get_access_token instance |> Lwt_r.map (fun token ->
					instance.token := Some token;
					token
				)
			in

			match !(instance.token) with
				| None -> refresh_token () |> Lwt_r.bind _post
				| Some token ->
					_post token |> LwtMonad.bind (function
						| Error (`Response (response, body)) when Response.status response = `Unauthorized ->
							consume_and_log body |> LwtMonad.bind refresh_token |> Lwt_r.bind _post
						| other -> Lwt.return other
					)

		let read_for_writing t path (consumer: Lock.proof -> ((string, Error.t) result Lwt_stream.t option, Error.t) result -> 'a Lwt.t) =
			let body = `Assoc ["keys", `List [key_of_path path ]] |> J.to_string in
			let url = t.url |> append_path ":lookup" in
			PathLock.acquire path (fun proof ->
				let%lwt response = post t ~body url
					|> LwtMonad.bind allowing_404
					|> LwtMonad.bind to_standard_error in

				let contents = Lwt.return response |> Lwt_r.bind (function
					| None -> Lwt.return (Ok None)
					| Some (_response, body) ->
						Body.to_string body |> Lwt.map parse_json |> Lwt.map (R.bindr (fun (json: J.json) ->
							let content = Keys.(json
								|> json_field found
								|> Option.bind json_singleton
								|> Option.bind (json_field entity)
								|> Option.bind (json_field properties)
								|> Option.bind (json_field content)
								|> Option.bind (json_field stringValue)
								|> Option.bind json_string
							) in
							match content with
								| Some content -> Ok (Some (Lwt_stream.of_list [Ok content]))
								| None ->
									(* lookup doesn't return a 404, it gives back a `missing` item *)
									let missing = json
										|> json_field Keys.missing
										|> Option.bind json_singleton
									in
									(match missing with
										| Some _ -> Ok None
										| _ -> Error (expected_json_field json Keys.(content ^ "." ^ stringValue))
									)
						))
				) in
				contents |> LwtMonad.bind (consumer proof)
			)

		let mutations lst =
			`Assoc ["mode", `String "NON_TRANSACTIONAL"; "mutations", `List lst]

		let write_s t path ?proof contents =
			(* write_s is not really streaming, since the API is JSON based *)
			let url = t.url |> append_path ":commit" in
			let%lwt contents = Lwt_stream.fold (fun chunk acc ->
				acc |> Option.bind (fun acc ->
					match chunk with
						| `Output chunk -> Some (acc ^ chunk)
						| `Rollback -> None
				)
			) contents (Some "") in
			(match contents with
				| None -> Lwt.return (Ok ())
				| Some contents ->
					let body = mutations Keys.([
						`Assoc [upsert, `Assoc [
							key, key_of_path path;
							properties, `Assoc [
								content, `Assoc [
									excludeFromIndexes, `Bool true;
									stringValue, `String contents;
								]
							]
						]]
					]) |> J.to_string in

					PathLock.acquire ?proof path (fun _proof ->
						post t ~body url |> LwtMonad.bind to_standard_error
							|> Lwt_r.bindM consume_and_log_body
					)
			)

		let delete t path =
			let body = mutations Keys.([
				`Assoc [delete, `Assoc [
					key, key_of_path path
				]]
			]) |> J.to_string in

			let url = t.url |> append_path ":commit" in
			post t ~body url |> LwtMonad.bind to_standard_error
				|> Lwt_r.bindM consume_and_log_body

		let reconnect _ _ = failwith "Reconnection not supported"
	end)

	include Impl
	include Dynamic_store.Augment(Impl)
end

module Cloud_datastore_unix = struct
	(* Will need to extract this into a module if we ever use it in mirage *)
	include Make(Cohttp_lwt_unix.Client)

	let connect = function
		| Cloud_datastore key_file ->
			let key_json = J.from_file key_file in
			let prop key = json_field key key_json |> Option.bind json_string |> Option.default_fn (fun () ->
				failwith "Invalid JSON"
			) in
			let url = Uri.of_string ("https://datastore.googleapis.com/v1/projects/" ^ (prop "project_id")) in
			let private_key = match prop "private_key" |> Cstruct.of_string |> X509.Encoding.Pem.Private_key.of_pem_cstruct1 with
				| `RSA key -> key
			in

			let ctx =
				(* appengine has crippled some unix syscalls for service / host resolution, so we need to roll our own resolvers *)
				let service proto =
					Lwt.return (match proto with
						| "http" -> Some { Resolver.name = "http"; port = 80; tls = false }
						| "https" -> Some { Resolver.name = "https"; port = 443; tls = true }
						| _ -> None
					)
				in
				let dns = Dns_resolver_unix.create () in
				let resolve svc uri = (match Uri.host uri with
					| None -> Lwt.return (`Unknown "No host")
					| Some host ->
						dns |> LwtMonad.bind (fun dns ->
							Dns_resolver_unix.gethostbyname dns host
						) |> Lwt.map (function
							| [] -> `Unknown ("Couldn't resolve host " ^ host)
							| addr :: _ -> `TCP (addr, svc.Resolver.port)
						)
				) in
				let resolver = Resolver_lwt.init ~service ~rewrites:["", resolve] () in
				(* Resolver_lwt.set_service ~f:service Resolver_lwt_unix.system; *)
				Cohttp_lwt_unix.Client.custom_ctx ~resolver:resolver ()
			in

			Ok {
				account = prop "client_email";
				ctx;
				private_key;
				token_uri = "https://www.googleapis.com/oauth2/v4/token";
				token = ref None;
				url;
			}

		| _ -> Error (`Invalid "connect")
end
