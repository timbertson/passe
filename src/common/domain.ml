open Re_compat

type t = string
let raw s = s

let hostname s =
	let uri = Uri.of_string s in
	match Uri.host uri with
		| Some host -> host
		| None -> s |> Str.replace_first (Str.regexp "/.*") ""

let known_common_slds =
	let result = ref None in
	fun () ->
		!result |> Option.default_fn (fun () ->
			let filename =
				try (Unix.getenv "SUPERGENPASS_DOMAINS")
				with Not_found -> raise (Failure "$SUPERGENPASS_DOMAINS not set")
			in
			lwt lines = Lwt_io.lines_of_file filename
				|> Lwt_stream.to_list in
			let rv = Lwt.return lines in
			result := Some rv;
			rv
		)

let re_dot = Str.regexp "\."

let guess s =
	let s = hostname s in
	let parts = Str.split (re_dot) s in
	let part = List.nth parts in
	let len = List.length parts in
	if len <= 2
	then Lwt.return s
	else (
		lwt known_common_slds = known_common_slds () in
		(* # Take the top-level and second-level domain *)
		let domain = part (len - 2) ^ "." ^ part (len - 1) in
		Lwt.return (
			(* # If this is one of the known second-level domains, take the third *)
			(* # level as well *)
			if List.mem domain known_common_slds
			then part (len - 3) ^ "." ^ domain
			else domain
		)
	)


