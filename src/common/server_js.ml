open Lwt
module Xhr = XmlHttpRequest
module J = Json_ext
module Version = Version.Make(Re_js)
module Server_common = Server_common.Make(Version)
module Logging = Logging.Make(Logging.Js_output)
include Server_common

let log = Logging.get_logger "sync"
exception Unsupported_protocol

let root_url =
	let open Url in
	ref (match Url.Current.get () with
		| Some (Http _ as u)
		| Some (Https _ as u) -> Url.string_of_url u |> Uri.of_string
		| None | Some (File _) -> raise Unsupported_protocol
	)

let json_content_type = "application/json"

let json_payload frame =
	let content_type = frame.Xhr.headers "content-type" in
	match content_type with
	| Some content_type when content_type = json_content_type -> (
			try
				Some (J.from_string frame.Xhr.content)
			with e -> (
				log#error "Failed to parse JSON: %s\n%s"
					frame.Xhr.content
					(Printexc.to_string e);
				None)
		)

	| Some other ->
			log#debug "Unexpected content-type: %s" other;
			None

	| None ->
			log#debug "No content-type given";
			None

let request ?content_type ?token ~meth ?data url =
	let (res, w) = Lwt.task () in
	let req = Xhr.create () in
	let url = Uri.to_string (canonicalize ~root:!root_url url) in
	req##_open (Js.string meth, Js.string url, Js._true);

	common_headers |> List.iter (fun (k,v) ->
		req##setRequestHeader (Js.string k, Js.string v)
	);

	content_type |> Option.may (fun content_type ->
		req##setRequestHeader (Js.string "Content-type", Js.string content_type)
	);

	token |> Option.may (fun token ->
		req##setRequestHeader (Js.string "Authorization",
			Js.string ("api-token t=" ^ (J.to_string token |> Uri.pct_encode)))
	);

	let headers s =
		Js.Opt.case
			(req##getResponseHeader (Js.bytestring s))
			(fun () -> None)
			(fun v -> Some (Js.to_string v))
	in

	req##onreadystatechange <- Js.wrap_callback (fun _ ->
		let open Xhr in
		(match req##readyState with
			| DONE ->
				(* If we didn't catch a previous event, we check the header. *)
				Lwt.wakeup w {
					url = url;
					code = req##status;
					content = Js.to_string req##responseText;
					content_xml = (fun () ->
						match Js.Opt.to_option (req##responseXML) with
						| None -> None
						| Some doc ->
						if (Js.some doc##documentElement) == Js.null
						then None
						else Some doc);
					headers = headers
				}
			| _ -> ())
	);

	begin match data with
		| Some d -> req##send(Js.some (Js.string d))
		| None -> req##send(Js.null)
	end;

	Lwt.on_cancel res (fun () -> req##abort ());
	res


let handle_json_response frame =
	log#info "got http response %d, content %s"
		frame.Xhr.code
		frame.Xhr.content
	;

	let payload = json_payload frame in
	let error = payload |> Option.bind (J.string_field "error") in
	return (match (frame.Xhr.code, payload) with
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
			let contents = match frame.Xhr.content with
				| "" -> "Request failed"
				| contents -> contents
			in

			Failed (code, error |> Option.default contents, response)
		)
	)

let post_json ?token ~(data:J.json) url =
	lwt frame = request
		?token
		~content_type:json_content_type
		~meth:"POST"
		~data:(J.to_string data)
		url in
	handle_json_response frame


let get_json ?token url =
	lwt frame = request
		?token
		~content_type:json_content_type
		~meth:"GET"
		url in
	handle_json_response frame
