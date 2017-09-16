open Passe
open Lwt
module Xhr = Lwt_xmlHttpRequest
module J = Json_ext
module Version = Version.Make(Re_js)

module Impl : Server.IMPL = struct
	let root_url =
		let open Url in
		match Url.Current.get () with
			| Some (Http _ as u)
			| Some (Https _ as u) -> Url.string_of_url u |> Uri.of_string
			| None | Some (File _) -> raise Server.Unsupported_protocol

	type headers = (string * string) list
	type response = string Xhr.generic_http_frame
	let init_headers () = []
	let set_header h k v = (k,v)::h

	let get_header k response = response.Xhr.headers k
	let response_body response = return response.Xhr.content
	let response_status response = response.Xhr.code

	let request ~headers ~meth ~data url =
		let (res, w) = Lwt.task () in
		let req = XmlHttpRequest.create () in
		let url = Uri.to_string url in
		let meth = Server.string_of_request_method meth in
		req##_open (Js.string meth) (Js.string url) (Js._true);

		headers |> List.iter (fun (k,v) ->
			req##setRequestHeader (Js.string k) (Js.string v)
		);

		let get_header key = Js.Opt.case
			(req##getResponseHeader (Js.bytestring key))
				(fun () -> None)
				(fun v -> Some (Js.to_string v))
		in

		req##.onreadystatechange := Js.wrap_callback (fun _ ->
			let open Xhr in
			(match req##.readyState with
				| XmlHttpRequest.DONE ->
					(* If we didn't catch a previous event, we check the header. *)
					Lwt.wakeup w {
						url = url;
						code = req##.status;
						content = Js.to_string req##.responseText;
						content_xml = (fun () ->
							match Js.Opt.to_option (req##.responseXML) with
							| None -> None
							| Some doc ->
							if (Js.some doc##.documentElement) == Js.null
							then None
							else Some doc);
						headers = get_header;
					}
				| _ -> ())
		);

		begin match data with
			| Some d -> req##send(Js.some (Js.string d))
			| None -> req##send(Js.null)
		end;

		Lwt.on_cancel res (fun () -> req##abort);
		res
end

include Server.Make(Version)(Impl)
