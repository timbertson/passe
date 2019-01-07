open Cohttp
open Cohttp_lwt
module Response = Cohttp.Response
open Passe
open Common
module J = Json_ext
module Log = (val Passe.Logging.log_module "cloud_datastore")

type datastore = {
	url : Uri.t;
}

(* split OK / Error responses without changing type *)
let result_of_response (response, body) =
	let status = Response.status response in
	Log.debug (fun m->m"HTTP response: %a" Response.pp_hum response);
	if status |> Code.code_of_status |> Code.is_success
		then Ok (response, body)
		else Error (response, body)

let consume_and_log body =
	(* Note: we always want to consume the body, as that eagerly cleans up resources *)
	Body.to_string body |> Lwt.map (fun body ->
		Log.debug (fun m->m "response body: %s" body)
	)

let consume_and_log_body (_response, body) = consume_and_log body

(* promote `Not_found to Ok None *)
let allowing_404 = function
	| Ok x -> Lwt.return (Ok (Some x))
	| Error (response, body) ->
			let status = Response.status response in
			if status = `Not_found
				then (consume_and_log body |> Lwt.map (fun () -> Ok None))
				else Lwt.return (Error (response, body))

(* turn (_, response) result into (_, Error.t) result *)
let to_standard_error = function
	| Ok x -> Lwt.return (Ok x)
	| Error (response, body) ->
		let status = Response.status response in
		consume_and_log body |> Lwt.map (fun () ->
			Error (`Failed ("HTTP response: " ^ (Code.string_of_status status)))
		)


module Make (Client:Cohttp_lwt.S.Client) = struct
	let kind = "doc"
	let content_key = "content"

	let append_path p u = Uri.with_path u (Uri.path u ^ p)

	let key_of_path path = `Assoc ["path", `List [
		`Assoc ["name", `String (Path.to_string path); "kind", `String kind]
	]]

	module Impl = (struct
		include Dynamic_store.Types
		type t = datastore

		module PathLock = Lock.Map(Path.Relative)( )

		let post ~body url =
			Log.debug (fun m->m"Posting to %a with body %s" Uri.pp_hum url body);
			Client.post ~body:(Body.of_string body) url
				|> Lwt.map result_of_response

		let read_for_writing { url } path (consumer: Lock.proof -> ((string, Error.t) result Lwt_stream.t option, Error.t) result -> 'a Lwt.t) =
			let body = `Assoc ["keys", `List [key_of_path path ]] |> J.to_string in
			let url = url |> append_path ":lookup" in
			PathLock.acquire path (fun proof ->
				let%lwt response = post ~body url
					|> LwtMonad.bind allowing_404
					|> LwtMonad.bind to_standard_error in

				let contents = Lwt.return response |> Lwt_r.bind (function
					| None -> Lwt.return (Ok None)
					| Some (_response, body) ->
						Body.to_string body |> Lwt.map (fun body ->
							try Ok (J.from_string body) with e -> Error (`Failed (Printexc.to_string e))
						) |> Lwt.map (R.bindr (function
							| `Assoc pairs -> (match List.assoc_opt content_key pairs with
								| Some `String content -> Ok (Some (Lwt_stream.of_list [Ok content]))
								| Some _ | None -> Error (`Invalid ("expected " ^ content_key ^ " string in response JSON"))
							)
							| _other -> Error (`Invalid "expected object")
						))
				) in
				contents |> LwtMonad.bind (consumer proof)
			)

		let write_s { url } path ?proof contents =
			(* write_s is not really streaming, since the API is JSON based *)
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
					let body = `Assoc ["mutations", `List [
						`Assoc ["upsert", `Assoc [
							"key", key_of_path path;
							"properties", `Assoc [
								content_key, `String contents
							]
						]]
					]] |> J.to_string in

					PathLock.acquire ?proof path (fun _proof ->
						post ~body url |> LwtMonad.bind to_standard_error
							|> Lwt_r.bindM consume_and_log_body
					)
			)

		let delete { url } path =
			let body = `Assoc ["mutations", `List [
				`Assoc ["delete", `Assoc [
					"key", key_of_path path
				]]
			]] |> J.to_string in

			post ~body url |> LwtMonad.bind to_standard_error
				|> Lwt_r.bindM consume_and_log_body

		let connect url = { url = Uri.of_string url }
		let reconnect _ = connect
	end)

	include Impl
	include Dynamic_store.Augment(Impl)
end
