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
			) state.db_signal last_sync_time
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
					lwt result = Client_auth.submit url data in
					let () = match result with
						| Left err -> set_error (Some err)
						| Right creds -> set_auth_state (Active_user creds)
					in
					return_unit
				in

				Lwt_js_events.clicks signup_button (fun event _ ->
					stop event;
					submit signup_url
				)
				<&>
				Lwt_js_events.submits elem (fun event _ ->
					stop event;
					submit login_url
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
				match_lwt post_json ~data:creds logout_url with
					| OK _ | Unauthorized _ ->
						set_auth_state Anonymous;
						return_unit;
					| Failed (msg,_) ->
						log#error "Can't log out: %s" msg;
						return_unit
			)
		) ()
	in

	state.auth_state |> S.map (fun auth ->
	log#info "Auth state: %s" (string_of_auth_state auth);

	match auth with
	| Anonymous -> login_form None
	| Failed_login username -> login_form (Some username)
	| Saved_user (username, creds) -> (
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
					match_lwt Server.post_json ~data:creds token_validate_url with
					| OK _ -> set_auth_state (Active_user (username, creds)); return_unit
					| Unauthorized msg ->
						log#warn "failed auth: %a" (Option.print print_string) msg;
						set_auth_state (Failed_login username);
						return_unit
					| Failed (msg, _) ->
						log#warn "unknown failure, assuming connectivity issue: %s" msg;
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
	| Active_user ((username, creds) as auth) -> (
		div ~cls:"account-status alert alert-success" ~children:[
			child span ~cls:"user" ~text:username ();
			logout_button "Log out" creds;
			child span ~cls:"online" ~children:[
				sync_state_widget auth;
			] ()
		]
		~mechanism:(fun elem ->
			let run_sync () = run_sync auth in
			lwt () = run_sync () in
			while_lwt true do
				log#info "sync loop running..";
				Lwt.pick [
					(* every 2 minutes, attempt a sync *)
					(Lwt_js.sleep 120.0 >>= run_sync);
					(effectful_stream_mechanism (sync_state |> S.map (function
						| Local_changes _ -> Lwt_js.sleep 2.0 >>= run_sync
						| _ -> return_unit
					)));
				]
			done
		) ()
	)
) |> Ui.stream
