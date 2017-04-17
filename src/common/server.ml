open Lwt
module J = Json_ext
open Common

module Server_common = struct
	type 'a generic_response =
		| OK of 'a
		| Unauthorized of string option
		| Failed of int * string * (J.json option)

	module Response = struct
		type 'a t = 'a generic_response

		let map : ('a -> 'b) -> 'a t -> 'b t = fun fn -> function
			| OK a -> OK (fn a)
			| Unauthorized _ as e -> e
			| Failed _ as e -> e

		let bind : ('a -> ('b, string) Rresult.result) -> 'a t -> 'b t =
			fun fn -> function
				| OK a -> (match (fn a) with
					| Rresult.Ok b -> OK b
					| Rresult.Error b -> Failed (0, b, None)
				)
				| Unauthorized _ as e -> e
				| Failed _ as e -> e

		let force : 'a t -> 'a = function
			| OK a -> a
			| Unauthorized e -> failwith (e |> Option.get "unauthorized")
			| Failed (_, msg, _) -> failwith msg

	end

	type response = J.json Response.t

	type 'a response_handler = J.json -> ('a, string) Rresult.result

	type request_method = [ `GET | `POST ]

end

module type Sig = sig
	include module type of Server_common

	type url = [
		| `Absolute of Uri.t
		| `Path of string list
		]

	val path : string list -> url
	val post_json : ?token:J.json -> data:J.json -> url -> response Lwt.t
	val get_json : ?token:J.json -> url -> response Lwt.t

	val root_url : Uri.t ref

	type 'result api

	val api : ?meth:request_method -> string list -> 'a response_handler -> 'a api

	val call : 'a api -> ?token:J.json -> J.json -> 'a generic_response Lwt.t
end

module Log = (val Logging.log_module "server")
open Server_common

let string_of_request_method = function
	| `GET -> "GET"
	| `POST -> "POST"

module type IMPL = sig
	val root_url : Uri.t

	type headers
	type response

	val get_header : string -> response -> string option
	val init_headers : unit -> headers
	val set_header : headers -> string -> string -> headers

	val response_body : response -> string Lwt.t
	val response_status : response -> int

	val request :
		headers:headers
		-> meth:request_method
		-> data:string option
		-> Uri.t
		-> response Lwt.t
end

exception Unsupported_protocol

module Make (Version:Version.Sig)(Impl:IMPL):Sig = struct
	include Server_common

	type url = [
		| `Absolute of Uri.t
		| `Path of string list
		]

	let path p = `Path p

	let root_url = ref Impl.root_url

	let canonicalize = function
		| `Absolute u -> u
		| `Path p -> Uri.with_path !root_url ("/" ^ String.concat "/" p)

	let common_headers = [
		"x-passe-version", Version.version;
	]

	let json_content_type = "application/json"

	let json_payload (response, body) =
		let content_type = Impl.get_header "content-type" response in
		match content_type with
		| Some content_type when content_type = json_content_type -> (
				try
					Some (J.from_string body)
				with e -> (
					Log.err (fun m->m "Failed to parse JSON: %s\n%s"
						body (Printexc.to_string e));
					None
				)
			)

		| Some other ->
				Log.debug (fun m->m "Unexpected content-type: %s" other);
				None

		| None ->
				Log.debug (fun m->m "No content-type given");
				None

	let handle_json_response response =
		let code = Impl.response_status response in
		Log.debug (fun m->m "got http response %d" code);

		lwt content = Impl.response_body response in
		Log.debug (fun m->m "got http body %s" content);
		let payload = json_payload (response, content) in
		let error = payload |> Option.bind (J.string_field "error") in
		return (match (code, payload) with
			| 401, json -> Unauthorized (
				json |> Option.bind (fun json ->
					json
					|> J.get_field "reason"
					|> Option.bind J.as_string)
				)
			| 200, (Some json as response) -> (
				match error with
					| Some error -> Failed (200, error, response)
					| None -> OK json
				)
			| code, response -> (
				let error_message =
					Option.default_fn (fun () ->
						Option.non_empty ~zero:"" content |> Option.default "Request failed"
					) error
				in

				Failed (code, error_message, response)
			)
		)

	let request ?content_type ?token ~meth ?data url =
		let url = canonicalize url in
		Log.info (fun m->m "Requesting: %s" (Uri.to_string url));

		let headers = ref (common_headers |> List.fold_left (fun headers (k,v) ->
			Impl.set_header headers k v
		) (Impl.init_headers ())) in

		content_type |> Option.may (fun content_type ->
			headers := Impl.set_header !headers "Content-type" content_type
		);

		token |> Option.may (fun token ->
			headers := Impl.set_header !headers
				"Authorization"
				("api-token t=" ^ (J.to_string token |> Uri.pct_encode))
		);

		Impl.request ~headers:!headers ~meth ~data url

	let post_json ?token ~(data:J.json) url =
		lwt response = request
			?token
			~content_type:json_content_type
			~meth:`POST
			~data:(J.to_string data)
			url in
		handle_json_response response

	let get_json ?token url =
		lwt response = request
			?token
			~content_type:json_content_type
			~meth:`GET
			url in
		handle_json_response response

	type 'result api = {
		meth: request_method;
		path: string list;
		handler: J.json -> ('result, string) result;
	}

	let api ?(meth=`POST) path handler = { meth; path; handler }

	let call api ?token data =
		let url = path (api.path) in
		request
			?token
			~content_type:json_content_type
			~meth:api.meth
			~data:(J.to_string data)
			url
		>>= handle_json_response
		|> Lwt.map (Response.bind api.handler)
end
