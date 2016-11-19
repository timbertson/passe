open Vdoml
open Passe
open Passe_js
open Lwt
open Js
open Dom_html
open Common
open React_ext
module List = List_ext
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
}

let eq a b =
	let {
		incognito = incognito_a;
		dialog = dialog_a;
		sync_state = sync_state_a;
		password_form = password_form_a;
		account_settings = account_settings_a;
	} = a in
	let {
		incognito = incognito_b;
		dialog = dialog_b;
		sync_state = sync_state_b;
		password_form = password_form_b;
		account_settings = account_settings_b;
	} = b in
	(
		incognito_a = incognito_b &&
		dialog_a = dialog_b &&
		sync_state_a = sync_state_b &&
		Password_form.eq password_form_a password_form_b &&
		account_settings_a = account_settings_b
	)

let string_of_dialog = function
	| `about -> "`about"
	| `account_settings -> "`account_settings"

let string_of_state = function { incognito; dialog; sync_state; password_form; _ } ->
	"{ incognito = " ^ (string_of_bool incognito) ^
	"; dialog = " ^ (Option.to_string string_of_dialog dialog) ^
	"; sync_state = " ^ (Sync_ui.string_of_state sync_state) ^
	"; password_form = " ^ (Password_form.string_of_state password_form) ^
	" }"

type message =
	| Toggle_incognito
	| Show_about_dialog
	| Show_account_settings
	| Dismiss_overlay
	| Sync of Sync_ui.internal_message
	| Password_form of Password_form.message
	| Account_settings of Account_settings.internal_message
	| Auth_state of Client_auth.auth_state
	| Hacky_state_override of t (* Remove me, obviously *)

let string_of_message = function
	| Toggle_incognito -> "Toggle_incognito"
	| Show_about_dialog -> "Show_about_dialog"
	| Show_account_settings -> "Show_account_settings"
	| Dismiss_overlay -> "Dismiss_overlay"
	| Sync msg -> "Sync " ^ (Sync_ui.string_of_message msg)
	| Password_form msg -> "Password_form " ^ (Password_form.string_of_message msg)
	| Auth_state auth -> "Auth_state " ^ (Client_auth.string_of_auth_state auth)
	| Account_settings msg -> "Account_settings " ^ (Account_settings.string_of_message msg)
	| Hacky_state_override _ -> "Hacky_state_override (...)"

let check cond = Printf.ksprintf (function s ->
	if cond then () else raise (AssertionError s)
	)

let is_within min max i = i >= min && i <= max
let within min max i = Pervasives.min (Pervasives.max i min) max

let logo () =
	let open Html in
	img ~src:"/res/images/footer.png" ~a:[a_class "footer-logo"] ()

let db_display sync : #Dom_html.element Passe_ui.widget =
	let contents:string signal = sync.Sync.stored_json |> S.map (fun json ->
		json
			|> Option.map (J.to_string)
			|> Option.default "<no DB>"
	) in

	let display = Passe_ui.div ~cls:"db"
		~children:[
			Passe_ui.text_stream contents
		] () in

	Passe_ui.div ~cls:"db-editor" ~children:[
		Passe_ui.frag display;
	] ()

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

let update ~sync ~storage_provider =
	let account_settings_signal = Account_settings.external_state sync in
	fun state message -> (
		Log.info (fun m->m "message: %s" (string_of_message message));
		let update_password_form = Password_form.update sync in
		let state = match message with
			| Toggle_incognito ->
				let incognito = not state.incognito in
				storage_provider#set_persistent incognito;
				{ state with incognito }
			| Show_about_dialog ->
				{ state with dialog = Some `about }
			| Show_account_settings ->
				{ state with dialog = Some `account_settings }
			| Dismiss_overlay ->
				{ state with dialog = None }
			| Auth_state auth ->
				let account_settings = auth
					|> Client_auth.authenticated_of_user_state
					|> Option.map (Account_settings.initial (S.value account_settings_signal))
				in
				{ state with account_settings }
			| Sync msg ->
				{ state with sync_state = Sync_ui.update state.sync_state msg }
			| Password_form msg ->
				{ state with password_form = update_password_form state.password_form msg }
			| Account_settings msg ->
				{ state with
					account_settings = state.account_settings
						|> Option.map (fun state -> Account_settings.update state msg);
				}
			| Hacky_state_override state -> state
		in
		Log.debug (fun m->m " -> state: %s" (string_of_state state));
		state
	)

type external_state = Sync_ui.external_state * Password_form.external_state * Account_settings.external_state
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

let initial : external_state -> t = fun (sync_state, password_form_state, _account_settings_state) ->
	{
		incognito = false;
		dialog = None;
		account_settings = None;
		sync_state = Sync_ui.initial sync_state;
		password_form = Password_form.initial password_form_state;
	}

(* XXX Hack for connecting multiple vdoml trees to the same state *)
let gen_updater ~sync ~storage_provider =
	let open Vdoml in
	let toplevel_ui_instances = ref [] in
	let update = update ~sync ~storage_provider in
	fun (tasks:(t, message) Ui.Tasks.t) -> (
		let instance = ref None in
		Ui.Tasks.sync tasks (fun inst ->
			toplevel_ui_instances := inst :: !toplevel_ui_instances;
			instance := Some inst
		);
		let update state message = (
			let state = update state message in
			match message with
				| Hacky_state_override _ -> state
				| _ -> (
					(* propagate to peers *)
					let () = match !instance with
						| None -> ()
						| Some instance -> !toplevel_ui_instances |> List.iter (fun dest ->
							(* phys equality *)
							if dest != instance then Ui.emit dest (Hacky_state_override state)
						)
					in
					state
				)
		) in
		update
	)

let view_overlay sync instance =
	let account_settings_panel = Ui.child ~message:account_settings_message
		(Account_settings.panel sync) instance in
	let overlay = Bootstrap.overlay ~cancel:Dismiss_overlay in
	(fun { dialog; account_settings; _ } ->
		let open Html in
		let inject_html elem =
			elem##innerHTML <- Js.string (
				About.aboutHtml ^ "\n<hr/><small>Version " ^ (Version.pretty ()) ^ "</small>";
			) in
		match dialog with
			| None -> text ""
			| Some `about ->
				overlay [
					Bootstrap.panel ~close:Dismiss_overlay ~title:"About Passé" [
						div [] |> Ui.hook ~create:inject_html
					]
				]
			| Some `account_settings ->
				account_settings
					|> Option.map account_settings_panel
					|> Option.default (text "Not logged in")
	)

let view sync instance =
	let open Html in
	let view_overlay = view_overlay sync instance in
	let view_sync = Ui.child
		~message:sync_ui_message
		(Sync_ui.component sync) instance in

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
		];
	]

let show_form ~storage_provider (sync:Sync.state) (container:Dom_html.element Js.t) =
	let module Tasks = Ui.Tasks in
	let external_state = external_state sync in
	let external_messages = external_state |> S.map external_messages in
	let initial_state = initial (S.value external_state) in
	let update = update ~sync ~storage_provider in

	let tasks = Tasks.init () in
	Tasks.async tasks (fun instance ->
		Bootstrap.install_overlay_handler instance Dismiss_overlay;
		external_messages |> S.changes |> Lwt_react.E.to_stream |> Lwt_stream.iter (fun messages ->
			messages |> List.iter (Ui.emit instance)
		)
	);

	let del child = Dom.removeChild container child in
	List.iter del (container##childNodes |> Dom.list_of_nodeList);

	let component = Ui.root_component ~eq ~update ~view:(view sync) initial_state in
	let all_content = Passe_ui.div ~children:[
		Passe_ui.frag (Passe_ui.vdoml ~tasks component)
	] () in
	Passe_ui.withContent container all_content (fun _ ->
		lwt () = Passe_ui.pause () in
		Lwt.return_unit
	)

let print_exc context e =
	Log.err (fun m->m "Uncaught %s Error: %s\n%s"
		context
		(Printexc.to_string e)
		(Printexc.get_callstack 20 |> Printexc.raw_backtrace_to_string)
	)

let () = Lwt.async_exception_hook := print_exc "Uncaught LWT"

let main ~storage_provider sync = (
	try_lwt (
		let main_elem = (document##getElementById (s"main")) in
		check (Opt.test main_elem) "main_elem not found!";
		let main_elem = Opt.get main_elem (fun _ -> raise Fail) in
		let offline_actions =
			if Lazy.force Passe_env_js.offline_access then [
				App_cache.update_monitor (fun () ->
					Log.info (fun m->m "appcache update ready");
					let busy = document##body##querySelector(Js.string"input:focus")
						|> Opt.to_option
						|> Option.map (fun elem ->
								let value = (Js.Unsafe.get elem (Js.string"value")) in
								value##length > 0
						) |> Option.default false
					in
					begin if busy then
						Log.warn (fun m->m "Not reloading; active input is nonempty")
					else
						Dom_html.window##location##reload()
					end;
					return_unit)
			] else (
				Log.info (fun m->m "Offline access disabled");
				[]
			)
		in
		Lwt.join ([ show_form ~storage_provider sync main_elem ] @ offline_actions)
	) with e -> (
		print_exc "Toplevel" e;
		return_unit
	)
)

let () = (
	Logging.set_reporter (Logs_browser.console_reporter ());
	Log.app (fun m->m "passe %s" (Version.pretty ()));
	let app_level, vdoml_level = (
		let uri = !Server.root_url in
		let open Logs in
		match Uri.fragment uri with
		| Some "trace" -> (Debug, Some Debug)
		| Some "debug" -> (Debug, Some Info)
		| Some "info" -> (Info, None)
		| _ -> (match Uri.host uri with
			| Some "localhost" -> (Info, None)
			| _ -> (Warning, None)
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
				main ~storage_provider Sync.(build {
					env_initial_auth = initial_auth;
					env_config_provider = config_provider
				})
			);
			false
		)
)

