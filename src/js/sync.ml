open Common
open React
open Ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest

let log = Logging.get_logger "sync"

let credentials = new Local_storage.record "credentials"
let credentials_signal =
	let c, update = S.create (credentials#get |> Option.default `Null) in
	credentials#watch update;
	c

let connected, set_connected = S.create false

let login_url = Server.path ["auth"; "login"]
let token_validate_url = Server.path ["auth"; "validate"]

let login_form () =
	let remember_me_input = input
	~attrs:[("type","checkbox");("checked","true")] () in
	let remember_me = signal_of_checkbox ~initial:true remember_me_input in

	let save_credentials creds =
		if S.value remember_me then
			credentials#save creds
	in

	let error, set_error = S.create None in

	let error_widget = error
		|> optional_signal_content (fun err -> Ui.div ~cls:"text-danger" ~text:err ())
		|> Ui.stream in

	form ~cls:"login" ~children:[
		error_widget;
		child label ~text:"User" ();
		child input ~attrs:[("name","user")] ();

		child label ~text:"password" ();
		child input
		~attrs:[("type","password"); ("name","password")] ();

		child label ~text:"remember me" ();
		frag remember_me_input;

		child input ~attrs:[("type","submit")] ();
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
			Console.console##log (elem);
			let data = `Assoc (Form.get_form_contents elem |> List.map (fun (name, value) -> (name, `String value))) in
			let open Server in
			match_lwt post_json ~data login_url with
			| OK json ->
					log#info "logged in!";
					save_credentials json;
					return_unit
			| Failed (message, _) ->
					set_error (Some message);
					return_unit
		)
	) ()

let presence_display: Ui.fragment_t = credentials_signal |> S.map (fun creds ->
	creds
		|> J.get_field "user"
		|> Option.bind J.as_string
		|> Option.map (fun user ->
				div ~cls:"userid" ~text:("Logged in as: " ^ user) ()
		)
) |> Ui.option_stream

let ui () = div ~children:[
	child h3 ~text:"server" ();
	presence_display;
	frag (login_form ());
] ()

let ui () = credentials_signal |> S.map (fun creds ->
	let user = creds
		|> J.get_field "user"
		|> Option.bind J.as_string in
	match user with
		| Some user -> (
				let online_text = connected |> S.map (fun connected ->
					let rv = if connected then
						span ~cls:"disconnected" ~children:[
							child i ~cls:"glyphicon glyphicon-ok" ();
							child span ~text:"connected" ();
						] ()
					else
						span ~cls:"offline" ~children:[
							child i ~cls:"glyphicon glyphicon-remove" ();
							child span ~cls:"link" ~text:"offline" ();
							child a ~cls:"link retry" ~children:[
								child i ~cls:"glyphicon glyphicon-refresh" ();
								child span ~text:"retry" ();
							] ();
						] ~mechanism:(fun elem ->
							let continue = ref true in
							while_lwt true do
								continue := false;
								let open Server in
								match_lwt Server.post_json ~data:creds token_validate_url with
								| OK _ -> set_connected true; return_unit
								| Failed (msg, json) ->
									log#warn "failed auth: %s" msg;
									let link = elem##querySelector("a") |> non_null in
									let reason = json |> Option.bind (J.get_field "reason") |> Option.bind J.as_string in
									match reason with
										| Some reason ->
												log#warn "credentials rejected: %s" reason;
												credentials#delete;
												return_unit
										| None ->
											continue := true;
											log#info "no reason given for rejection; assuming connectivity issue";
											pick [
												(Lwt_js.sleep 6.0);
												(
													lwt (_:Dom_html.mouseEvent Js.t) = Lwt_js_events.click link in
													return_unit
												);
											]
							done
						) ()
					in
					(rv:>Dom.node widget_t)
				) |> stream in
				let container = div ~cls:"account-status alert" ~children:[
					child span ~cls:"user" ~text:user ();
					frag (online_text);
				] () in
				container#class_s "alert-success" connected;
				container#class_s "alert-warning" (S.map not connected);
				container
		)
		| None -> div ~children:[
			frag (login_form ())
		] ()
) |> Ui.stream
