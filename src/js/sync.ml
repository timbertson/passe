open Common
open React
open Ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest

let log = Logging.get_logger "sync"

let credentials = Config.field "credentials"

let username_key = "user"
let login_url = Server.path ["auth"; "login"]
let token_validate_url = Server.path ["auth"; "validate"]

type username = string
type auth_token = J.json
type auth_state =
	| Anonymous
	| Expired_user of username
	| Saved_user of username * auth_token
	| Active_user of username * auth_token

type date = int64
type sync_state =
	| Unknown
	| Updated of date
	| Syncing
	| Local_changes

let sync_state, set_sync_state = S.create Unknown

let saved_auth_state = credentials#signal |> S.map (fun conf ->
		conf |> Option.bind (fun token -> token
			|> J.get_field username_key
			|> Option.bind (J.as_string)
			|> Option.map (fun (u:string) -> Saved_user (u, token))
		) |> Option.default Anonymous
	)

let auth_state, set_auth_state =
	let signal,update = S.create (S.value saved_auth_state) in
	let effect = signal |> S.map update in
	ignore @@ S.retain signal (fun () -> ignore effect; ());
	(signal, update)


let remember_me_input = input ~attrs:[("type","checkbox");("checked","true")] ()
let remember_me = signal_of_checkbox ~initial:true remember_me_input

(* wrap set_auth_state aith automatic updating of localStorage *)
let set_auth_state state = begin match state with
	| Anonymous -> credentials#delete
	| Active_user (_, creds) ->
			if S.value remember_me then
				credentials#save creds
	| _ -> ()
	end;
	set_auth_state state

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
			child input ~cls:"btn btn-primary" ~attrs:[("type","submit"); ("value","Sign in")] ();
			child div ~cls:"form-group form-group-sm email" ~children:[
				child label ~cls:"sr-only" ~text:"User" ();
				child input ~cls:"form-control" ~attrs:[
					("name","user");
					("placeholder","User");
					("type","text");
					("value", Option.default "" username);
				] ();
			] ();
			space;

			child div ~cls:"form-group form-group-sm password" ~children:[
				child label ~cls:"sr-only" ~text:"password" ();
				child input ~cls:"form-control" ~attrs:[
					("type","password");
					("name","password");
					("placeholder","password")
				] ();
			] ();

			space;

			child div ~cls:"checkbox remember-me form-group" ~children:[
				frag remember_me_input;
				space;
				child label ~text:"Remember me" ();
			] ();

			child div ~cls:"clearfix" ();
		] ~mechanism:(fun elem ->
			effectful_stream_mechanism (remember_me
				|> S.map (fun remember_me ->
						log#info "remember me changed to: %b" remember_me;
						if (not remember_me) then (
							credentials#delete
						)
				)
			)
			<&>
			Lwt_js_events.submits elem (fun event _ ->
				stop event;
				log#info "form submitted";
				let data = `Assoc (Form.get_form_contents elem |> List.map (fun (name, value) -> (name, `String value))) in
				let open Server in
				set_error None;
				lwt response = post_json ~data login_url in
				begin match response with
				| OK creds ->
					let username = creds
						|> J.get_field username_key
						|> Option.bind J.as_string in
					(match username with
						| Some user -> set_auth_state (Active_user (user, creds))
						| None -> raise (AssertionError "credentials doesn't contain a user key")
					)

				| Failed (message, _) -> set_error (Some message)
				end;
				return_unit
			)
		) ()
	] ()

let ui () =
	auth_state |> S.map (fun auth -> match auth with
	| Anonymous -> login_form None
	| Expired_user username -> login_form (Some username)
	| Saved_user (username, creds) -> (
		let offline_message, set_offline_message = S.create
			(span ~text:"connecting..." ()) in

		div ~cls:"account-status alert alert-warning" ~children:[
			child span ~cls:"user" ~text:username ();
			child span ~cls:"offline" ~children:[
				stream offline_message;
			] ~mechanism:(fun offline ->
				let continue = ref true in
				while_lwt !continue do
					continue := false;
					let open Server in
					match_lwt Server.post_json ~data:creds token_validate_url with
					| OK _ -> set_auth_state (Active_user (username, creds)); return_unit
					| Failed (msg, json) ->
						log#warn "failed auth: %s" msg;
						let reason = json |> Option.bind (J.get_field "reason") |> Option.bind J.as_string in
						match reason with
							| Some reason ->
									log#warn "credentials rejected: %s" reason;
									set_auth_state Anonymous;
									credentials#delete;
									return_unit
							| None ->
								continue := true;
								set_offline_message (
									span ~children:[
										child i ~cls:"glyphicon glyphicon-remove" ();
										child span ~text:"offline" ();
									] ());
								log#info "no reason given for rejection; assuming connectivity issue";
								Ui.withContent offline ?before:(offline##firstChild |> Js.Opt.to_option) (a ~cls:"link retry disabled" ~children:[
									child i ~cls:"glyphicon glyphicon-refresh" ();
									child span ~text:"retry" ();
								] ()) (fun elem ->
									pick [
										(
											lwt (_:Dom_html.mouseEvent Js.t) = Lwt_js_events.click elem in
											return_unit
										);
										(Lwt_js.sleep 60.0);
									]
								)
				done
			) ()
		] ()
	)
	| Active_user (username, _) -> (
		div ~cls:"account-status alert alert-success" ~children:[
			child span ~cls:"user" ~text:username ();
			child span ~cls:"online" ~children:[
				child i ~cls:"glyphicon glyphicon-ok" ();
				child span ~text:"online" ();
			] ()
		] ()
	)
) |> Ui.stream
