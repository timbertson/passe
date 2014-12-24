open Passe
open Common
open React_ext
open Ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest
open Sync

let ui state =
	(* let config_provider, set_config_provider = S.create (Lazy.force ephemeral_config) in *)
	let last_sync_signal = state.last_sync#signal in
	let last_sync_time = last_sync_signal |> signal_lift_opt (function
		| `Float t -> t
		| _ -> raise @@ AssertionError ("invalid `last_sync` value")
	) in

	(* let remember_me_input = input ~attrs:[("type","checkbox");("checked","true")] () in *)
	(* let remember_me = signal_of_checkbox ~initial:true remember_me_input in *)
	let set_auth_state = state.set_auth_state in

	let sync_running = state.sync_running in
	let run_sync = state.run_sync in

	let sync_state = S.bind sync_running (fun is_running ->
		if is_running then S.const Syncing else (
			S.l2 (fun db sync_time ->
				let len = List.length db.Store.changes in
				if len = 0 then
					sync_time |> Option.map (fun t -> Updated_at t) |> Option.default Uptodate
				else Local_changes len
			) state.db_fallback last_sync_time
		)) in


	let login_form (username:string option) =
		let error, set_error = S.create None in

		let error_widget = error
			|> optional_signal_content (fun err -> Ui.p ~cls:"text-danger" ~text:err ~children:[
				child i ~cls:"glyphicon glyphicon-remove" ();
			] ())
			|> Ui.stream in
		
		div ~cls:"account-status login alert alert-info" ~children:[
			child form ~cls:"login form-inline" ~attrs:[("role","form")] ~children:[
				error_widget;
				child div ~cls:"form-group form-group-xs email" ~children:[
					child label ~cls:"sr-only" ~text:"User" ();
					child input ~cls:"form-control" ~attrs:[
						("name","user");
						("placeholder","User");
						("type","text");
						("value", Option.default "" username);
					] ();
				] ();
				space;

				child div ~cls:"form-group form-group-xs password" ~children:[
					child label ~cls:"sr-only" ~text:"password" ();
					child input ~cls:"form-control" ~attrs:[
						("type","password");
						("name","password");
						("placeholder","password")
					] ();
				] ();

				space;
				child input ~cls:"btn btn-default muted signup pull-right" ~attrs:[("type","button"); ("value","Register")] ~text:"Register" ();
				child input ~cls:"btn btn-primary" ~attrs:[("type","submit"); ("value","Sign in")] ();

				child div ~cls:"clearfix" ();
			] ~mechanism:(fun elem ->
				let signup_button = elem##querySelector(".signup") |> non_null in
				let submit url =
					log#info "form submitted";
					let data = `Assoc (Form.get_form_contents elem
						|> List.map (fun (name, value) -> (name, `String value))) in
					let open Server in
					let open Either in
					set_error None;
					lwt response = Server.post_json ~data url in
					let open Server in
					let () = match response with
						| OK response ->
							set_auth_state (Auth.Active_user (Auth.get_response_credentials response))
						| Failed (_, message, _) ->
								set_error (Some message)
						| Unauthorized _ -> assert false
					in
					return_unit
				in

				Lwt_js_events.clicks signup_button (fun event _ ->
					stop event;
					submit Client_auth.signup_url
				)
				<&>
				Lwt_js_events.submits elem (fun event _ ->
					stop event;
					submit Client_auth.login_url
				)
			) ()
		] ()
	in

	let sync_state_widget auth =
		let open Ui in
		let _node w = (w:>Dom.node widget_t) in
		let sync_mech = (fun elem ->
			Lwt_js_events.clicks elem (fun event _ ->
				lwt () = run_sync auth in
				return (App_cache.update ())
			)
		) in
		sync_state |> S.map (function
			| Uptodate -> _node @@ empty ()
			| Updated_at date ->
					let now = Date.time () in
					let time_diff = max 0.0 (now -. date) in
					_node @@ a ~cls:"link has-tooltip"
						~attrs:[("title", "Last sync " ^ (Date.human_time_span_desc time_diff) ^ " ago")]
						~children:[icon "refresh"]
						~mechanism:sync_mech ()
			| Syncing ->
				_node @@ span
					~cls:"syncing"
					~children:[icon "refresh"] ()
			| Local_changes count ->
					_node @@ span
						~attrs:[("title", (string_of_int count) ^ " pending changes...")]
						~children:[
					child a
						~cls:"link"
						~children:[icon "upload"]
						~mechanism:sync_mech ()
				] ()
		) |> stream
	in

	let logout_button tooltip creds = child a
		~cls:"hover-reveal link"
		~attrs:["title",tooltip]
		~children:[icon "remove"]
		~mechanism:(fun elem ->
			Lwt_js_events.clicks elem (fun evt _ ->
				let open Server in
				match_lwt post_json ~data:creds Client_auth.logout_url with
					| OK _ | Unauthorized _ ->
						set_auth_state Client_auth.Anonymous;
						return_unit;
					| Failed (_, msg,_) ->
						log#error "Can't log out: %s" msg;
						return_unit
			)
		) ()
	in

	let account_settings_button username creds = child a
		~cls:"hover-reveal link settings-button"
		~attrs:["title", "Settings"]
		~children:[icon "cog"]
		~mechanism:(fun elem ->
			Lwt_js_events.clicks elem (fun evt _ ->
				Ui.overlay (fun close ->
					Ui.panel ~close ~title:"Account settings" ~children:[
						child div ~children:[
							(
								let error, set_error = S.create None in
								let error_widget = error
									|> optional_signal_content (fun err -> Ui.p ~cls:"text-danger" ~text:err ~children:[
										icon "remove";
									] ())
									|> Ui.stream in
								let db = S.value state.db_fallback in
								let current_length = Store.((get_defaults db).default_length) in
								let length, set_length = S.create current_length in
								let set_length str =
									match (try Some (int_of_string str) with _ -> None) with
										| Some len -> set_length len
										| None -> set_error (Some "Not a number")
								in
								let length_field = input_of_signal ~update:set_length (S.map string_of_int length) in
								length_field#attr "class" "form-control";

								child form ~cls:"form-horizontal" ~attrs:["action","/fail";"method","POST"] ~children:[
									error_widget;

									row `XS ~cls:"form-group" [
										col ~size:4 [control_label "Default password length"];
										col [frag length_field];
									];

									row `XS [
										col ~size:8 ~offset:4 [
											child input ~cls:"btn btn-primary" ~attrs:[("type","submit"); ("value","Save defaults")] ();
										];
									];
								] ~mechanism:(fun form ->
									Lwt_js_events.submits form (fun event _ ->
										stop event;
										let length = S.value length in
										let saved = if length = current_length
											then true
											else Sync.save_default ~state (`Length length)
										in
										if saved then (
											Dom_html.window##alert (Js.string "Defaults saved");
											close ();
										) else (
											Dom_html.window##alert (Js.string "Unable to save DB");
										);
										return_unit
									)
								) ();

							);

							child hr ();

							(
								let error, set_error = S.create None in
								let error_widget = error
									|> optional_signal_content (fun err -> Ui.p ~cls:"text-danger" ~text:err ~children:[
										child i ~cls:"glyphicon glyphicon-remove" ();
									] ())
									|> Ui.stream in
								let password_input ~label name =
									let attrs = ["name",name; "type","password"] in
									row `XS ~cls:"form-group" [
										col ~size:4 [control_label label];
										col [child input ~cls:"form-control" ~attrs:attrs ()];
									]
								in

								child form ~cls:"form-horizontal" ~attrs:["action","/fail";"method","POST"] ~children:[
									error_widget;
									password_input ~label:"Old password" "new";
									password_input ~label:"New password" "new";
									password_input ~label:"New password (again)" "new2";

									row `XS [
										col ~size:8 ~offset:4 [
											child input ~cls:"btn btn-primary" ~attrs:[("type","submit"); ("value","Change password")] ();
										]
									];
								] ~mechanism:(fun form ->
									Lwt_js_events.submits form (fun event _ ->
										stop event;
										let pairs = Form.get_form_contents form in
										let data = `Assoc (pairs |> List.map (fun (a, b) -> a, `String b)) in
										let new1 = data |> J.mandatory J.string_field "new"
										and new2 = data |> J.mandatory J.string_field "new2" in
										if new1 <> new2 then (
											set_error (Some "Passwords don't match");
											return_unit
										) else (
											let open Server in
											match_lwt post_json ~token:creds ~data Client_auth.change_password_url with
												| OK creds ->
													Dom_html.window##alert (Js.string "Password changed.");
													set_auth_state (Auth.Active_user (username, creds));
													close ();
													return_unit
												| Unauthorized msg ->
													set_error (Some (msg |> Option.default "Unauthorized"));
													return_unit;
												| Failed (_, msg,_) ->
													set_error (Some msg);
													return_unit
										)
									)
								) ()
							);

							child hr ();

							(
								let error, set_error = S.create None in
								let error_widget = error
									|> optional_signal_content (fun err -> Ui.p ~cls:"text-danger" ~text:err ~children:[
										child i ~cls:"glyphicon glyphicon-remove" ();
									] ())
									|> Ui.stream in
								let password, set_password = S.create "" in
								let password_field = input_of_signal ~update:set_password password in
								let () = (
									password_field#attr "type" "password";
									password_field#attr "class" "form-control";
								) in

								child form ~cls:"form-horizontal" ~attrs:["action","/fail";"method","POST"] ~children:[
									error_widget;
									row `XS ~cls:"form-group" [
										col ~size:4 [control_label "Password"];
										col [frag password_field ];
									];

									row `XS ~collapse:true [
										col ~size:8 ~offset:4 ~cls:"text-right" [
											child span ~cls:"text-muted" ~text:"Careful now... " ();
											child input ~cls:"btn btn-danger" ~attrs:[("type","submit"); ("value","Delete account")] ();
										];
									];
								] ~mechanism:(fun form ->
									Lwt_js_events.submits form (fun event _ ->
										stop event;
										if (Dom_html.window##confirm (Js.string "Are you SURE?") |> Js.to_bool) then begin
											set_error None;
											let open Server in
											match_lwt post_json
												~token:creds
												~data:(`Assoc ["password", `String (S.value password)])
												Client_auth.delete_user_url
											with
												| OK _ ->
													set_auth_state Auth.Anonymous;
													close ();
													return_unit;
												| Unauthorized msg ->
													set_error (Some (msg |> Option.default "Unauthorized"));
													return_unit;
												| Failed (_, msg,_) ->
													set_error (Some msg);
													return_unit
										end else return_unit
									)
								) ();
							)
						] ()
					] ()
				)
			)
		) ()
	in


	state.auth_state |> S.map (fun auth ->
	log#info "Auth state: %s" (Client_auth.string_of_auth_state auth);

	match auth with
	| Client_auth.Anonymous -> login_form None
	| Client_auth.Failed_login username -> login_form (Some username)
	| Client_auth.Saved_user (username, creds) -> (
		let busy, set_busy = S.create true in

		let offline_message = span ~text:"offline " () in
		offline_message#class_s "hidden" busy;

		let sync_button = a ~children:[icon "refresh"] () in
		sync_button#class_s "syncing" busy;
		sync_button#class_s "link" (S.map not busy);

		div ~cls:"account-status alert alert-warning" ~children:[
			child span ~cls:"user" ~text:username ();
			logout_button "Log out" creds;
			child span ~cls:"offline" ~children:[
				frag offline_message;
				frag sync_button;
			] ~mechanism:(fun elem ->
				let continue = ref true in
				while_lwt !continue do
					set_busy true;
					continue := false;
					let open Server in
					match_lwt Server.post_json ~data:creds Client_auth.token_validate_url with
					| OK _ -> set_auth_state (Client_auth.Active_user (username, creds)); return_unit
					| Unauthorized msg ->
						log#warn "failed auth: %a" (Option.print print_string) msg;
						set_auth_state (Client_auth.Failed_login username);
						return_unit
					| Failed (_, msg, _) ->
						log#warn "unknown failure: %s" msg;
						continue := true;
						set_busy false;
						Lwt.pick [
							(
								lwt (_:Dom_html.mouseEvent Js.t) = Lwt_js_events.click elem in
								return_unit
							);
							(Lwt_js.sleep 60.0);
						]
				done
			) ()
		] ()
	)
	| Client_auth.Active_user ((username, creds) as auth) -> (
		div ~cls:"account-status alert alert-success" ~children:[
			child span ~cls:"user" ~text:username ();
			logout_button "Log out" creds;
			child span ~cls:"online" ~children:[
				account_settings_button username creds;
				sync_state_widget auth;
			] ()
		]
		~mechanism:(fun elem ->
			let run_sync () = run_sync auth in
			lwt () = run_sync () in
			while_lwt true do
				log#info "sync loop running..";
				Lwt.pick [
					(* every 30 minutes, attempt a sync *)
					(Lwt_js.sleep 18000.0 >>= run_sync);
					(* also sync shortly after making a change to the DB *)
					(effectful_stream_mechanism (sync_state |> S.map (function
						| Local_changes _ -> Lwt_js.sleep 2.0 >>= run_sync
						| _ -> return_unit
					)));
				]
			done
		) ()
	)
) |> Ui.stream
