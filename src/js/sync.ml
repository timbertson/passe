open Common
open React
open Ui
open Lwt
module J = Json_ext
module Xhr = XmlHttpRequest

let log = Logging.get_logger "sync"

let credentials = new Local_storage.record "credentials"
let credentials_signal =
	let c, update = S.create `Null in
	credentials#watch update;
	c

let login_url = Server.path ["auth"; "login"]

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
		child label ~text:"username" ();
		child input ~attrs:[("name","username")] ();

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
			| Failed message ->
					set_error (Some message);
					return_unit
		)
	) ()

let presence_display: Ui.fragment_t = credentials_signal |> S.map (fun (creds:J.json) ->
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
