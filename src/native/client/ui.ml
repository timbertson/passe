open Passe
open Passe_unix
open Common
open Lwt
open Lwt_react

module Log = (val Logging.log_module "ui")

class password_prompt term text = object(self)
	inherit LTerm_read_line.read_password ()
	inherit [Zed_utf8.t] LTerm_read_line.term term
	initializer
		self#set_prompt (S.const (LTerm_text.of_string text))
end

class plain_prompt term text = object(self)
	inherit LTerm_read_line.read_line ()
	inherit [Zed_utf8.t] LTerm_read_line.term term

	method! show_box = false

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

let output_password ~use_clipboard ~term ~domain text =
	lwt () = LTerm.flush term in
	lwt copied = if use_clipboard then copy_to_clipboard text else return false in
	if copied then (
		Logs.warn (fun m->m "  (copied to clipboard)");
		Lwt.return_unit
	) else (
		Logs.warn (fun m->m "  Generated password for %s:\n  " domain);
		LTerm.printl text
	)

module Input_map = Zed_input.Make(LTerm_key)

let edit_and_save ~sync_state ~domain ~existing ~term () : bool Lwt.t =
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
	let title_row = new LTerm_widget.hbox in
	let verb = match existing with Some _ -> "Editing" | None -> "Creating new" in
	let title = new LTerm_widget.label (verb ^ " domain " ^ domain.domain) in
	title_row#add title;
	frame#add (title_row);

	let e_domain = add_field ~label:"Domain: " domain.domain in
	let e_note = add_field   ~label:"Note:   " (domain.note |> Option.default "") in
	let e_length = add_field ~label:"Length: " (string_of_int domain.length) in
	let e_suffix = add_field ~label:"Suffix: " (domain.suffix |> Option.default "") in
	frame#add (new LTerm_widget.hbox);


	let error_row = new LTerm_widget.hbox in
	let error_message = new LTerm_widget.label "" in
	error_row#add error_message;
	frame#add (error_row);

	let action_row = new LTerm_widget.hbox in
	let instructions = new LTerm_widget.label "<return> to save, <esc> to cancel" in
	action_row#add instructions;
	frame#add (action_row);

	let edited = ref None in

	(* Exit when the user presses Ctrl+X *)
	let waiter, wakener = wait () in
	let cancel () = wakeup wakener (); true in
	frame#on_event (function
		| LTerm_event.Key { LTerm_key.control = false; meta = false; shift = false; code = LTerm_key.Escape } -> cancel ()
		| LTerm_event.Key { LTerm_key.control = true; meta = false; shift = false; code = LTerm_key.Char ch } when ch = UChar.of_char 'c' -> cancel ()
		| LTerm_event.Key { LTerm_key.control = false; meta = false; shift = false; code = LTerm_key.Enter } ->
			begin
			Log.debug (fun m->m "Saving changes");
			try
				edited := Some (Store.({
					domain = e_domain#text;
					note = Option.non_empty ~zero:"" e_note#text;
					suffix = Option.non_empty ~zero:"" e_suffix#text;
					length = int_of_string e_length#text;
				}));
				wakeup wakener ();
				true
			with e -> (
				error_message#set_text ("Error: " ^ (Printexc.to_string e));
				false
			)
			end
		| _ -> false
	);


	lwt () = LTerm_widget.run term frame waiter in
	
	match !edited with
		| Some edited ->
			if Sync.save_change
				~state:sync_state
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

let delete ~sync_state ~(existing:Store.domain) ~term () =
	let open Store in
	lwt text = (new plain_prompt term ("Really delete "^(existing.domain)^"? (Y/n) "))#run in
	match text with
		| "" | "y" | "Y" ->
			if Sync.save_change ~state:sync_state ~original:(Some (Domain existing)) None
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
		Sync.login state ~user ~password
	in

	try_lwt
		lwt authenticated_user =
			let open Client_auth in
			let validate_implicit_user () =
				lwt u = validate_implicit_auth state in
				return (match u with
					| `Implicit_user _ as u -> u
					| `Anonymous -> failwith "Server is not authenticated"
				)
			in

			let rec loop (auth_state:auth_state) : authenticated_user_state Lwt.t =
				match auth_state with
					| `Active_user _ as u -> return u
					| `Saved_user _ as u -> validate_explicit_auth state u >>= loop
					| `Logged_out -> login_prompt None >>= loop
					| `Failed_login username -> login_prompt (Some username) >>= loop

					(* Note: implicit auth types never loop, as there's nothing the
					 * user can do to incluence their result *)
					| `Implicit_user _ as u -> return u
					| `Saved_implicit_user _  | `Anonymous -> validate_implicit_user ()
			in
			loop (S.value state.auth_state)
		in

		state.Sync.run_sync authenticated_user |> Lwt.map (R.map (fun () ->
			Log.app (fun m->m "Sync completed successfully")
		))
	with e -> (
		return (Error (Printexc.to_string e))
	)

let lterm_reporter term =
	let ppf, flush =
		let b = Buffer.create 255 in
		let flush () = let s = Buffer.contents b in Buffer.clear b; s in
		Format.formatter_of_buffer b, flush
	in

	let lterm_print _src _level ~over k msgf =
		let k _ =
			Lwt_main.run (
				let contents = flush () in
				LTerm.fprint term contents >> LTerm.flush term
			);
			over ();
			k ()
		in
		msgf @@ fun ?header ?tags:_tags fmt ->
		match header with
		| None -> Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
		| Some h -> Format.kfprintf k ppf ("[%s] @[" ^^ fmt ^^ "@]@.") h
	in
	Lwt.return Logs.({ report = lterm_print })

let main ~domain ~edit ~one_time ~use_clipboard ~env () =
	let sync_state = Sync.build env in
	lwt term = Lazy.force LTerm.stderr in
	lwt reporter = lterm_reporter term in
	Logging.set_reporter reporter;
	let user_db () = S.value (sync_state.Sync.db_signal) in
	let db_fallback () = S.value (sync_state.Sync.db_fallback) in

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
		let domain_text = Domain.guess domain |> Option.force in
		let get_stored () = user_db () |> Option.bind (Store.lookup domain_text) in
		let get_domain db stored = stored |> Option.default (Store.default db domain_text) in

		let open Store in
		let rec post_generate_actions (stored_domain:Store.domain option) =
			let db = db_fallback () in
			let domain = get_domain db stored_domain in
			let continue () = post_generate_actions (get_stored ()) in
			let next (_:bool) = continue () in
			let edit_and_save () =
				edit_and_save ~sync_state ~domain ~existing:stored_domain ~term () >>= next
			in
			let delete existing () =
				delete ~sync_state ~existing ~term () >>= next
			in
			let try_sync () =
				lwt sync_result = sync_ui sync_state in
				let () = match sync_result with
					| Ok () -> ()
					| Error e -> Log.err (fun m->m "%s" e)
				in
				continue ()
			in

			let common_actions = [
				("c", "c: Continue", break);
				("q", "q: Quit", fun () -> Lwt.return ());
				("r", "r: Regenerate", input_loop ~domain:(Some domain_text));
				("s", "s: Sync", try_sync);
			] in
			let actions = match S.value (sync_state.Sync.current_user_db) with
				| None -> common_actions
				| Some _ -> common_actions @ (match stored_domain with
					| Some existing ->
						[
							("e", "e: Edit " ^ domain_text, edit_and_save);
							("d", "d: Delete " ^ domain_text, delete existing);
						]
					| None ->
						[
							("a", "a: Add " ^ domain_text ^ " to database", edit_and_save);
						]
				)
			in
			let rec ask () =
				lwt response = (new plain_prompt term (
						"\n" ^ (
							actions
							|> List.map (fun (_, text, _) -> " " ^ text)
							|> String.concat "\n")
						^ "\n -- What next? [c] "))#run in
				let response = if response = "" then "c" else response in
				Log.debug (fun m->m "response: [%s]" response);
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

		let db = db_fallback () in
		let stored_domain = get_stored () in
		let domain = get_domain db stored_domain in
		if edit then
			lwt edited = edit_and_save ~sync_state ~domain ~existing:stored_domain ~term () in
			if edited then return ()
			else exit 1
		else begin
			Log.app (fun m->m " - Length: %d" domain.length);
			begin match stored_domain with
				| None ->
						Log.app (fun m->m " - This is a new domain.")
				| Some domain ->
						domain.suffix |> Option.may (fun suffix -> (Log.app (fun m->m " - Suffix: %s" suffix)));
						domain.note |> Option.may (fun note -> (Log.app (fun m->m " - Note: %s" note)));
			end;

			lwt password = (new password_prompt term ("Password for " ^ domain_text ^ ": "))#run in
			let generated = Password.generate ~domain password in
			lwt () = output_password ~use_clipboard ~term ~domain:domain_text generated in
			if one_time
				then Lwt.return_unit
				else post_generate_actions stored_domain
		end
	in


	try_lwt
		input_loop ~domain ()
	with
		| LTerm_read_line.Interrupt
		| Sys.Break -> exit 1
	finally
		LTerm.flush term

let list_domains env domain =
	let sync_state = Sync.build env in
	let db = S.value (sync_state.Sync.db_fallback) in
	begin match domain with
		| Some domain -> (match Store.lookup domain db with
			(* lookup specific domain *)
			| Some domain ->
					print_endline (domain |> Store.json_string_of_domain);
					true
			| None ->
					prerr_endline "Domain not found";
					false
		)
		| None ->
			(* just dump all domains *)
			Store.get_records db
				|> Store.StringMap.keys
				|> List.sort compare
				|> List.iter print_endline
			;
			true
	end

