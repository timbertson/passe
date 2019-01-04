open Vdoml
open Passe
open Passe_js
open Lwt
open Js
open Dom_html
open Common
open React_ext
module J = Json_ext
module Log = (val Logging.log_module "main")

let s = Js.string

exception Fail
type dialog_type = [ `about | `account_settings ]
type t = {
	incognito : bool;
	dialog: dialog_type option;
	sync_state: Sync_ui.state;
	password_form: Password_form.state;
	account_settings: Account_settings.state option;
	show_debug : bool;
}

let eq a b =
	let {
		incognito = incognito_a;
		dialog = dialog_a;
		sync_state = sync_state_a;
		password_form = password_form_a;
		account_settings = account_settings_a;
		show_debug = show_debug_a;
	} = a in
	let {
		incognito = incognito_b;
		dialog = dialog_b;
		sync_state = sync_state_b;
		password_form = password_form_b;
		account_settings = account_settings_b;
		show_debug = show_debug_b;
	} = b in
	(
		incognito_a = incognito_b &&
		dialog_a = dialog_b &&
		sync_state_a = sync_state_b &&
		Password_form.eq password_form_a password_form_b &&
		account_settings_a = account_settings_b &&
		show_debug_a = show_debug_b
	)

let string_of_dialog = function
	| `about -> "`about"
	| `account_settings -> "`account_settings"

let string_of_state = function { incognito; dialog; sync_state; password_form; account_settings; _ } ->
	"{ incognito = " ^ (string_of_bool incognito) ^
	"; dialog = " ^ (Option.to_string string_of_dialog dialog) ^
	"; sync_state = " ^ (Sync_ui.string_of_state sync_state) ^
	"; password_form = " ^ (Password_form.string_of_state password_form) ^
	"; account_settings = " ^ (Option.to_string Account_settings.string_of_state account_settings) ^
	" }"

type message =
	| Toggle_incognito
	| Show_about_dialog
	| Show_account_settings
	| Dismiss_overlay
	| Sync of Sync_ui.internal_message
	| Password_form of Password_form.message
	| Account_settings of Account_settings.internal_message

let string_of_message = function
	| Toggle_incognito -> "Toggle_incognito"
	| Show_about_dialog -> "Show_about_dialog"
	| Show_account_settings -> "Show_account_settings"
	| Dismiss_overlay -> "Dismiss_overlay"
	| Sync msg -> "Sync " ^ (Sync_ui.string_of_message msg)
	| Password_form msg -> "Password_form " ^ (Password_form.string_of_message msg)
	| Account_settings msg -> "Account_settings " ^ (Account_settings.string_of_message msg)

let check cond = Printf.ksprintf (function s ->
	if cond then () else Error.raise_assert s
)

let is_within min max i = i >= min && i <= max
let within min max i = Pervasives.min (Pervasives.max i min) max

let logo () =
	let open Html in
	img ~src:"/res/images/footer.png" ~a:[a_class "footer-logo"] ()

let db_display db =
	let open Html in
	div ~a:[a_class "db-editor"] [
		div ~a:[a_class "db"] [
			text (Store.to_json_string db);
		]
	]

let view_footer incognito =
	let open Html in
	let open Bootstrap in

	let toggle response = emitter ~response Toggle_incognito in
	let incognito_checkbox = input ~a:[
		a_input_type `Checkbox;
		a_onclick (toggle `Unhandled);
		a_checked incognito;
		a_title "Don't store anything on this browser";
	] () in

	let incognito_container = div ~a:[
		a_class_list (List.filter_map identity [
			Some "incognito-checkbox";
			(if incognito then Some "selected" else None);
		]);
	] [
		incognito_checkbox;
		span ~a:[a_onclick (toggle `Handled)] [text" Incognito mode"];
	] in

	row `XS [
		col [
			ul ~a:[a_class "list-unstyled"] [
				li [
					incognito_container;
				];
			];
		];
		col ~cls:"text-right" [
			ul ~a:[a_class "list-unstyled"] [
				li ~a:[
					a_class "link";
					a_onclick (emitter Show_about_dialog);
				] [
					text "About this site";
				]
			];
		];
	]

(* map component messages into toplevel messages,
 * removing messages which need to be handled by this component *)
let sync_ui_message (msg:Sync_ui.message) = match msg with
	| `show_account_settings -> Show_account_settings
	| #Sync_ui.internal_message as msg -> Sync msg

let password_form_message (msg:Password_form.message) = Password_form msg

let account_settings_message (msg:Account_settings.message) = match msg with
	| `hide -> Dismiss_overlay
	| #Account_settings.internal_message as msg -> Account_settings msg

let reset_account_settings auth_state account_settings_state =
	auth_state
		|> Client_auth.authenticated_of_user_state
		|> Option.map (Account_settings.initial (account_settings_state))

let update ~sync ~storage_provider =
	let account_settings_signal = Account_settings.external_state sync in
	let auth_signal = sync.Sync.auth_state in
	fun state message -> (
		Log.info (fun m->m "message: %s" (string_of_message message));
		let state = match message with
			| Toggle_incognito ->
				let incognito = not state.incognito in
				storage_provider#set_persistent incognito;
				{ state with incognito }
			| Show_about_dialog ->
				{ state with dialog = Some `about }
			| Show_account_settings ->
				let account_settings = reset_account_settings (S.value auth_signal) (S.value account_settings_signal) in
				{ state with dialog = Some `account_settings; account_settings }
			| Dismiss_overlay ->
				{ state with dialog = None }
			| Sync msg ->
				{ state with sync_state = Sync_ui.update state.sync_state msg }
			| Password_form msg ->
				{ state with password_form = Password_form.update state.password_form msg }
			| Account_settings msg ->
				{ state with
					account_settings = state.account_settings
						|> Option.map (fun state -> Account_settings.update state msg);
				}
		in
		Log.debug (fun m->m " -> state: %s" (string_of_state state));
		state
	)

let command ~show_debug ~sync instance =
	let do_async = Ui.async instance in
	let sync_ui_command = Sync_ui.command ~sync ~do_async ~emit:(Ui.emit instance % sync_ui_message) in
	let password_form_command = Password_form.command ~sync ~show_debug ~emit:(Ui.emit instance % password_form_message) in
	(fun state message ->
		match message with
			| Sync msg -> sync_ui_command state.sync_state msg
			| Password_form msg -> password_form_command state.password_form msg
			| _ -> None
	)


type external_state =
	Sync_ui.external_state
	* Password_form.external_state
	* Account_settings.external_state

let external_state sync : external_state React.signal = S.l3 (fun a b c -> a,b,c)
	(Sync_ui.external_state sync)
	(Password_form.external_state sync)
	(Account_settings.external_state sync)

let external_messages ((sync, password_form, account_settings):external_state) : message list =
	List.concat [
		(Sync_ui.external_messages sync |> List.map sync_ui_message);
		(Password_form.external_messages password_form |> List.map password_form_message);
		(Account_settings.external_messages account_settings |> List.map account_settings_message);
	]

let initial ~show_debug : external_state -> t = fun (
		sync_state,
		password_form_state,
		_account_settings_state
	) ->
		let domain_text = "" in
	{
		show_debug;
		incognito = false;
		dialog = None;
		account_settings = None;
		sync_state = Sync_ui.initial sync_state;
		password_form = Password_form.initial password_form_state domain_text;
	}

let view_overlay sync instance =
	let account_settings_panel = Ui.child ~message:account_settings_message
		(Account_settings.panel sync) instance in
	let overlay = Bootstrap.overlay instance ~cancel:Dismiss_overlay in
	(fun { dialog; account_settings; _ } ->
		let open Html in
		let inject_html elem =
			elem##.innerHTML := Js.string (
				About.aboutHtml ^ "\n<hr/><small>Version " ^ (Version.pretty ()) ^ "</small>";
			) in
		dialog |> Option.map (fun dialog ->
			overlay [(match dialog with
			| `about ->
				Bootstrap.panel ~close:Dismiss_overlay ~title:"About Passé" [
					div [] |> Ui.hook ~create:inject_html
				]
			| `account_settings ->
				account_settings
					|> Option.map account_settings_panel
					|> Option.default_fn (fun () ->
						Bootstrap.panel ~close:Dismiss_overlay ~title:"Error" [
							text "You are not logged in."
						]
					)
					
			)]
		) |> Option.default empty
	)

let view sync instance =
	let open Html in
	let view_overlay = view_overlay sync instance in
	let view_sync = Ui.child
		~message:sync_ui_message
		Sync_ui.component instance in

	let view_password_form = Ui.child
		~message:password_form_message
		Password_form.component instance in

	fun state -> div [
		view_overlay state;
		div ~a:[a_class "container main"] [
			view_sync state.sync_state;
			view_password_form state.password_form;
		];
		div [
			div ~a:[a_class "container footer"] [
				view_footer state.incognito
			];
			div ~a:[a_class "container"] [ logo () ];
			if state.show_debug
				then db_display state.password_form.Password_form.db
				else empty
			;
		];
	]

let component ~show_debug ~tasks ~storage_provider (sync:Sync.state) =
	let external_state = external_state sync in
	let external_messages = external_state |> S.map external_messages in
	let initial_state = initial ~show_debug (S.value external_state) in
	let update = update ~sync ~storage_provider in
	let command = command ~sync ~show_debug in

	Ui.Tasks.async tasks (fun instance ->
		external_messages |> S.changes |> Lwt_react.E.to_stream |> Lwt_stream.iter (fun messages ->
			messages |> List.iter (Ui.emit instance)
		)
	);
	Ui.root_component ~eq ~update ~command ~view:(view sync) initial_state

let print_exc context e =
	Log.err (fun m->m "Uncaught %s Error: %s\n%s"
		context
		(Printexc.to_string e)
		(Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string)
	)

let () = Lwt.async_exception_hook := print_exc "Uncaught LWT"

let main ~show_debug ~storage_provider sync = (
	let tasks = Ui.Tasks.init () in
	if Lazy.force Passe_env_js.offline_access
		then Ui.Tasks.async tasks (fun _instance ->
			App_cache.update_monitor (fun () ->
				Log.info (fun m->m "appcache update ready");
				let busy = document##.body##querySelector (Js.string"input:focus")
					|> Opt.to_option
					|> Option.map (fun elem ->
							let value = (Js.Unsafe.get elem (Js.string"value")) in
							value##.length > 0
					) |> Option.default false
				in
				begin if busy then
					Log.warn (fun m->m "Not reloading; active input is nonempty")
				else
					Dom_html.window##.location##reload
				end;
				return_unit)
		)
		else Log.info (fun m->m "Offline access disabled");
	Ui.main ~tasks ~root:"main" (component ~show_debug ~tasks ~storage_provider sync) ()
)

let () = (
	Logging.set_reporter (Logs_browser.console_reporter ());
	Log.app (fun m->m "passe %s" (Version.pretty ()));
	let app_level, vdoml_level, show_debug = (
		let uri = !Server.root_url in
		let open Logs in
		match Uri.fragment uri with
		| Some "trace" -> (Debug, Some Debug, true)
		| Some "debug" -> (Debug, Some Info, true)
		| Some "info" -> (Info, None, false)
		| _ -> (match Uri.host uri with
			| Some "localhost" -> (Info, None, false)
			| _ -> (Warning, None, false)
		)
	) in
	Logs.set_level ~all:true (Some app_level);
	vdoml_level |> Option.default app_level |> Ui.set_log_level;

	let storage_provider = (new Local_storage.provider (true)) in
	let config_provider = Config.build storage_provider in
	let initial_auth = Sync.initial_auth_state (Lazy.force Passe_env_js.auth_mode) in

	let listener = ref null in
	listener := Opt.return @@ Dom_events.listen
		window
		(Event.make "DOMContentLoaded")
		(fun _ _ ->
			Opt.iter !listener Dom_events.stop_listen;
			Lwt.async (fun () ->
				initial_auth >>= fun initial_auth ->
				main ~show_debug ~storage_provider Sync.(build {
					env_initial_auth = initial_auth;
					env_config_provider = config_provider
				})
			);
			false
		)
)

