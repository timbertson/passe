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

let result_of_response (response, body) =
	let status = Response.status response in
	Log.debug (fun m->m"HTTP response: %a" Response.pp_hum response);
	if status |> Code.code_of_status |> Code.is_success
		then Lwt.return (Ok (Some (response, body)))
		else (
			let%lwt body = Body.to_string body in
			Log.debug (fun m->m "response body: %s" body);
			Lwt.return (if status = `Not_found
				then Ok None
				else Error (`Failed ("HTTP response: " ^ (Code.string_of_status status)))
			)
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
			Client.post ~body:(Body.of_string body) url |> LwtMonad.bind result_of_response

		let read_for_writing { url } path (consumer: Lock.proof -> ((string, Error.t) result Lwt_stream.t option, Error.t) result -> 'a Lwt.t) =
			let body = `Assoc ["keys", `List [key_of_path path ]] |> J.to_string in
			let url = url |> append_path ":lookup" in
			PathLock.acquire path (fun proof ->
				let%lwt response = post ~body url in
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
						post ~body url |> Lwt_r.bind (function
							| Some (_response, body) ->
								(* Note: we always want to consume the body, as that eagerly cleans up resources *)
								let%lwt consumed_body = Body.to_string body in
								Log.debug (fun m->m "response body: %s" consumed_body);
								Lwt.return (Ok ())
							| None -> Lwt.return (Error (`Failed "Not_found"))
						)
					)
			)

		let delete { url } path =
			let body = `Assoc ["mutations", `List [
				`Assoc ["delete", `Assoc [
					"key", key_of_path path
				]]
			]] |> J.to_string in

			post ~body url |> Lwt_r.bind (function
				| Some (_response, body) ->
					(* Note: we always want to consume the body, as that eagerly cleans up resources *)
					let%lwt consumed_body = Body.to_string body in
					Log.debug (fun m->m "response body: %s" consumed_body);
					Lwt.return (Ok ())
				| None -> Lwt.return (Error (`Failed "Not_found"))
			)

		let connect url = { url = Uri.of_string url }
		let reconnect _ = connect
	end)

	include Impl
	include Dynamic_store.Augment(Impl)
end
