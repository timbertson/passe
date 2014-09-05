open Lwt
module J = Json_ext
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Client.Response
module Header = Cohttp.Header
module Body = Cohttp_lwt_body

let log = Logging.get_logger "sync"
exception Unsupported_protocol

type response =
	| OK of J.json
	| Unauthorized of string option
	| Failed of string * (J.json option)

let root_url =
	(* XXX *)
	Uri.of_string "http://localhost:8080/"

let path p = Uri.with_path root_url (String.concat "/" p)


let json_content_type = "application/json"

let json_payload (response, body) =
	let content_type = Header.get (Response.headers response) "content-type" in
	match content_type with
	| Some content_type when content_type = json_content_type -> (
			try
				return (Some (J.from_string body))
			with e -> (
				log#error "Failed to parse JSON: %s\n%s"
					body
					(Printexc.to_string e);
				return None)
		)

	| Some other ->
			log#debug "Unexpected content-type: %s" other;
			return None

	| None ->
			log#debug "No content-type given";
			return None

let request ?content_type ?token ~meth ?data url =
	let headers = ref (Header.init ()) in
	content_type |> Option.may (fun content_type ->
		headers := Header.add !headers "Content-type" content_type
	);

	token |> Option.may (fun token ->
		headers := Header.add !headers "Authorization" ("api-token t=" ^ (J.to_string ~std:true token |> Uri.pct_encode))
	);

	let body = Option.map Body.of_string data in
	let headers = !headers in

	log#info "Requesting: %s" (Uri.to_string url);
	Client.call ~headers ?body meth url

let handle_json_response (response, body) =
	let code = Response.status response |> Cohttp.Code.code_of_status in
	log#info "got http response %d" code ;

	lwt content = Body.to_string body in
	lwt payload = json_payload (response, content) in
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
				| Some error -> Failed (error, response)
				| None -> OK json
			)
		| code, response -> (
			let contents = match content with
				| "" -> "Request failed"
				| contents -> contents
			in

			Failed (error |> Option.default contents, response)
		)
	)

let post_json ?token ~(data:J.json) url =
	lwt frame = request
		?token
		~content_type:json_content_type
		~meth:`POST
		~data:(J.to_string ~std:true data)
		url in
	(* lwt frame = Xhr.perform ~post_args:data url in *)
	handle_json_response frame


let get_json ?token url =
	lwt frame = request
		?token
		~content_type:json_content_type
		~meth:`GET
		url in
	handle_json_response frame
