open Passe
module J = Json_ext
module Re = Re_native
module Version = Version.Make(Re)

module Impl : Server.IMPL = struct
	module Client = Cohttp_lwt_unix.Client
	module Response = Cohttp_lwt_unix.Response
	module Header = Cohttp.Header
	module Body = Cohttp_lwt_body

	let default_root = try Unix.getenv "PASSE_SERVER" with Not_found -> "https://passe.gfxmonk.net/"
	let root_url =
		(* XXX take from config *)
		Uri.of_string default_root

	type headers = Header.t
	type response = Response.t * Body.t

	let init_headers = Header.init
	let set_header = Header.add
	let get_header key (response, _body) =
		Header.get (Response.headers response) key

	let response_status (response, _body) =
		Response.status response |> Cohttp.Code.code_of_status

	let response_body (_response, body) = Body.to_string body

	let request ~headers ~meth ~data uri =
		let body = Option.map Body.of_string data in
		Client.call ~headers ?body (meth:>Cohttp.Code.meth) uri
end

include Server.Make(Version)(Impl)
