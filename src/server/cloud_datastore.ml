open Cohttp
open Cohttp_lwt
open Passe
open Common
module J = Json_ext

type datastore = {
	url : Uri.t;
}

let result_of_response (response, body) =
	let status = Response.status response in
	if status |> Code.code_of_status |> Code.is_success
		then Ok (Some (response, body))
		else (
			if status = `Not_found
				then Ok None
				else Error (`Failed ("HTTP response: " ^ (Code.string_of_status status)))
		)

module Make (Client:Cohttp_lwt.S.Client) = struct
	let kind = "doc"

	let append_path p u = Uri.with_path u (Uri.path u ^ p)

	module Impl = (struct
		include Dynamic_store.Types
		type t = datastore

		module PathLock = Lock.Map(Path.Relative)( )

		let read_for_writing { url } path (consumer: Lock.proof -> ((string, Error.t) result Lwt_stream.t option, Error.t) result -> 'a Lwt.t) =
			let body = `Assoc ["keys", `List [
				`Assoc ["path", `List [
					`Assoc ["name", `String (Path.to_string path); "kind", `String kind]
				]]
			]] |> J.to_string |> Body.of_string in
			let url = url |> append_path ":lookup" in
			PathLock.acquire path (fun proof ->
				let%lwt response = Client.post ~body url |> Lwt.map result_of_response in
				let contents = response |> R.bindr (function
					| None -> Ok None
					| Some (response, body) ->
						let headers = (response |> Response.headers) in
						let content_length = Header.get headers "Content-Length"
							|> Option.map (fun s -> try Ok (int_of_string s) with _ -> Error (`Failed ("invalid content-length: "^s)))
							|> Option.default_fn (fun () -> Error (`Failed "Content length missing"))
						in
						content_length |> R.map (fun expected_len ->
							let body_chunks = body |> Cohttp_lwt.Body.to_stream in
							let seen_length = ref 0 in
							let error_result = ref (Some (Error (`Failed "Content-length mismatch"))) in
							Some (Lwt_stream.from (fun () ->
								Lwt_stream.get body_chunks |> Lwt.map (function
									| Some chunk ->
										seen_length := !seen_length + (String.length chunk);
										Some (Ok chunk)
									| None ->
										(* EOF, check we read exactly `expected_len` bytes: *)
										if !seen_length = expected_len then
											None
										else (
											let result = !error_result in
											error_result := None; (* only return an error once *)
											result
										)
								)
							))
						)
				) in
				consumer proof contents
			)

		let write_s = Obj.magic

		let delete = Obj.magic
		let connect url = { url = Uri.of_string url }
		let reconnect _ = connect
	end)

	include Impl
	include Dynamic_store.Augment(Impl)
end
