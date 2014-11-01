open Passe
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

module Input_map = Zed_input.Make(LTerm_key)

let edit_and_save ~sync_state ~db ~domain ~existing ~term () =
	let open CamomileLibraryDyn.Camomile in (* ??? *)
	let frame = new LTerm_widget.vbox in

	let add_field ~label initial =
		let line = new LTerm_widget.hbox in
		line#add ~expand:false (new LTerm_widget.label label);
		let editor = new LTerm_edit.edit () in
		Zed_edit.insert editor#context (Zed_rope.of_string initial);
		line#add editor;
		frame#add ~expand:false line;
		editor
	in

	let open Store in
	let e_domain = add_field ~label:"Domain: " domain.domain in
	let e_length = add_field ~label:"Length: " (string_of_int domain.length) in
	let e_suffix = add_field ~label:"Suffix: " (domain.suffix |> Option.default "") in
	let e_hint = add_field   ~label:"Hint:   " (domain.hint |> Option.default "") in
	frame#add (new LTerm_widget.hbox);

	let edited = ref None in

	(* Exit when the user presses Ctrl+X *)
	let waiter, wakener = wait () in
	let cancel () = wakeup wakener (); true in
	frame#on_event (let open LTerm_key in function
		| LTerm_event.Key { LTerm_key.control = false; meta = false; shift = false; code = LTerm_key.Escape } -> cancel ()
		| LTerm_event.Key { LTerm_key.control = true; meta = false; shift = false; code = LTerm_key.Char ch } when ch = UChar.of_char 'c' -> cancel ()
		| LTerm_event.Key { LTerm_key.control = false; meta = false; shift = false; code = LTerm_key.Enter } ->
			log#debug "Saving changes";
			begin try
				edited := Some (Store.({
					domain = e_domain#text;
					hint = Option.non_empty ~zero:"" e_hint#text;
					suffix = Option.non_empty ~zero:"" e_suffix#text;
					length = int_of_string e_length#text;
				}));
			with e -> Lwt_main.run (LTerm.printl "Error!")
			end;
			wakeup wakener ();
			true
		| _ -> false
	);


	let action_row = new LTerm_widget.hbox in
	let instructions = new LTerm_widget.label "<return> to save, <esc> to cancel" in
	action_row#add instructions;
	frame#add (action_row);

	lwt () = LTerm_widget.run term frame waiter in
	
	match !edited with
		| Some edited ->
			if Sync.save_change
				~db ~state:sync_state
				~original:(existing |> Option.map (fun d -> Domain d))
				(Some (Domain edited))
			then
				lwt () = LTerm.fprintlf term "Saved %s" domain.domain in
				return true
			else
				return false
		| None ->
			lwt () = LTerm.fprintlf term "Cancelled." in
			return false

let delete ~sync_state ~db ~(existing:Store.domain) ~term () =
	let open Store in
	lwt text = (new plain_prompt term ("Really delete "^(existing.domain)^"? (Y/n) "))#run in
	match text with
		| "" | "y" | "Y" ->
			if Sync.save_change ~db ~state:sync_state ~original:(Some (Domain existing)) None
			then
				lwt () = LTerm.fprintlf term "Deleted %s" existing.domain in
				return true
			else
				return false
		| _ ->
			lwt () = LTerm.fprintlf term "Cancelled" in
			return false

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

	try_lwt
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
	with e -> (
		log#error "Sync failed: %s" (Printexc.to_string e);
		raise e
	)

let main ~domain ~length ~quiet ~use_clipboard ~config () =
	let sync_state = Sync.build config in
	let db : Store.t option = let open Client_auth in match (S.value sync_state.Sync.auth_state) with
		| Active_user (user, _) | Saved_user (user, _) ->
			let stored = Sync.local_db_for_user config user in
			stored#get |> Option.map Store.parse_json
		| _ -> None
	in
	let db = ref db in
	lwt term = Lazy.force LTerm.stderr in
	Logging.current_writer := (fun dest str -> Lwt_main.run (LTerm.fprint term str >> LTerm.flush term));

	(* unbind default actions we don't want *)
	Input_map.bindings !LTerm_edit.bindings |> List.iter (let open LTerm_key in function
		| ([{control = false; meta = false; shift = false; code = Enter}] as b, _)
		| ([{control = false; meta = false; shift = false; code = Tab}] as b, _)
		| ([{control = false; meta = false; shift = true; code = Tab}] as b, _)
		| ([{control = false; meta = false; shift = false; code = Down}] as b, _)
		| ([{control = false; meta = false; shift = false; code = Up}] as b, _)
			-> LTerm_edit.unbind b
		| _ -> ()
	);


	let rec input_loop ~domain () =
		let break = input_loop ~domain:None in

		lwt domain = match domain with
			| Some d -> return d
			| None -> (new plain_prompt term "Domain: ")#run
		in
		lwt domain_text = (Domain.guess domain) in
		let stored_domain = !db |> Option.bind (Store.lookup domain)
		in

		let open Store in
		let domain = match stored_domain with
			| None -> Store.default domain_text
			| Some domain ->
					log#log " - Length: %d" domain.length;
					domain.suffix |> Option.may (log#log " - Suffix: %s");
					domain.hint |> Option.may (log#log " - Hint: %s");
					domain
		in

		let domain = match length with Some l -> {domain with length = l} | None -> domain in
		log#debug "Length: %d" domain.length;

		lwt password = (new password_prompt term ("Password for " ^ domain_text ^ ": "))#run in
		let generated = Password.generate ~domain password in
		lwt () = output_password ~use_clipboard ~quiet ~term ~domain:domain_text generated in

		let rec post_generate_actions () =
			let continue = post_generate_actions in
			let next is_done = if is_done then break () else continue () in
			let edit_and_save () =
				edit_and_save ~sync_state ~db ~domain ~existing:stored_domain ~term () >>= next
			in
			let delete existing () =
				delete ~sync_state ~db ~existing ~term () >>= next
			in

			match !db with
				| None -> Lwt.return_unit
				| Some current_db ->
					let actions = [
						("",  "c: Continue", break);
						("q", "q: Quit", fun () -> Lwt.return ());
						("r", "r: Sync", fun () -> sync_ui sync_state >>= continue);
					] @ (match stored_domain with
						| Some existing ->
								[
									("e", "e: Edit " ^ domain_text, edit_and_save);
									("d", "d: Delete " ^ domain_text, delete existing);
								]
						| None ->
								[
									("s", "s: Save " ^ domain_text, edit_and_save);
								]
					) in

					let rec ask () =
						lwt response = (new plain_prompt term (
								"\n" ^ (
									actions
									|> List.map (fun (_, text, _) -> " " ^ text)
									|> String.concat "\n")
								^ "\n -- What next? [c] "))#run in
						let response = if response = "" then "c" else response in
						log#debug "response: [%s]" response;
						lwt () = LTerm.fprintlf term "" in
						let action =
							try actions
								|> List.find (fun (key, _, _) -> key = response)
								|> fun (_,_,action) -> action
							with Not_found -> (fun () ->
								lwt () = LTerm.fprintlf term "Huh?" in
								ask ()
							) in
						lwt () = action () in
						(* XXX unclean, grows stack *)
						return ()
					in ask ()
				in
				post_generate_actions ()
	in

	try_lwt
		input_loop ~domain ()
	with
		| LTerm_read_line.Interrupt
		| Sys.Break -> exit 1
	finally
		LTerm.flush term

