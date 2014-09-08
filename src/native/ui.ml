open Common
open Lwt
open Lwt_react
open LTerm_style

let log = Logging.get_logger "ui"

class password_prompt term text = object(self)
	inherit LTerm_read_line.read_password () as super
	inherit [Zed_utf8.t] LTerm_read_line.term term
	initializer
		self#set_prompt (S.const (LTerm_text.of_string text))
end

class plain_prompt term text = object(self)
	inherit LTerm_read_line.read_line () as super
	inherit [Zed_utf8.t] LTerm_read_line.term term

	method show_box = false

	initializer
		self#set_prompt (S.const (LTerm_text.of_string text))
end

let copy_to_clipboard text =
	match_lwt Lwt_process.with_process_out ("", [|"pyperclip"; "-i"|]) (fun proc ->
		lwt () = Lwt_io.write proc#stdin text in
		proc#close
	) with
	| Unix.WEXITED 0 -> return_true
	| _ -> return_false

let output_password ~use_clipboard ~term ~quiet ~domain text =
	lwt () = LTerm.flush term in

	lwt copied = if use_clipboard then copy_to_clipboard text else return false in
	if copied then (
		if quiet then return_unit else
			LTerm.fprintlf term "  (copied to clipboard)"
	) else (
		lwt () =
			if quiet then return_unit else
				LTerm.fprintf term "  Generated password for %s:\n  " domain
		in
		LTerm.printl text
	)

let main ~domain ~length ~quiet ~use_clipboard ~config () =
	let sync_state = Sync.build config in
	let db : Store.t option = let open Client_auth in match (S.value sync_state.Sync.auth_state) with
		| Active_user (user, _) | Saved_user (user, _) ->
			let stored = Sync.local_db_for_user config user in
			stored#get |> Option.map Store.parse_json
		| _ -> None
	in
	lwt term = Lazy.force LTerm.stderr in
	Logging.current_writer := (fun dest str -> Lwt_main.run (LTerm.fprint term str >> LTerm.flush term));
	try_lwt
		lwt domain = match domain with
			| Some d -> return d
			| None -> (new plain_prompt term "Domain: ")#run
		in
		lwt domain_text = (Domain.guess domain) in
		let domain = db |> Option.bind (Store.lookup domain)
		in

		let open Store in
		let domain = match domain with
			| None -> Store.default domain_text
			| Some domain ->
					domain.suffix |> Option.may (log#log " - Suffix: %s");
					log#debug "Digest: %s" (domain.digest |> string_of_digest);
					domain.hint |> Option.may (log#log " - Hint: %s");
					domain
		in

		let domain = match length with Some l -> {domain with length = l} | None -> domain in
		log#debug "Length: %d" domain.length;

		lwt password = (new password_prompt term ("Password for " ^ domain_text ^ ": "))#run in
		let generated = Password.generate ~domain password in
		output_password ~use_clipboard ~quiet ~term ~domain:domain_text generated

	with
		| LTerm_read_line.Interrupt
		| Sys.Break -> exit 1
	finally
		LTerm.flush term

let sync_ui state =
	lwt term = Lazy.force LTerm.stderr in
	let open Sync in

	let login_prompt user =
		lwt user = match user with
			| Some existing ->
				lwt text = (new plain_prompt term ("Username ["^existing^"]: "))#run in
				return (if text = "" then existing else text)
			| None -> (new plain_prompt term "Username: ")#run
		in
		lwt password = (new password_prompt term "Password: ")#run in
		lwt auth = Sync.login state ~user ~password in
		return auth
	in

	lwt credentials =
		let rec loop auth_state =
			let open Auth in
			match auth_state with
				| Active_user creds -> return creds
				| Saved_user creds ->
						validate_credentials state creds >>= loop
				| Anonymous ->
						login_prompt None >>= loop
				| Failed_login username ->
						login_prompt (Some username) >>= loop
		in
		loop (S.value state.auth_state)
	in

	lwt () = state.Sync.run_sync credentials in
	log#info "Sync completed successfully";
	return_unit
