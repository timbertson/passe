open Passe
open Passe_js
open Common
open React_ext
open Vdoml
open Html
open Bootstrap
open Js_util
module Keycode = Keycode_ext

module Log = (val Logging.log_module "domain_form")
type modified_record = {
	original: Store.domain;
	current: Store.domain;
}

type record =
	| New of Store.domain
	| Saved of Store.domain
	| Modified of modified_record

let string_of_record = function
	| New d -> "New " ^ (Store.json_string_of_domain d)
	| Saved d -> "Saved " ^ (Store.json_string_of_domain d)
	| Modified { original; current } -> "Modified " ^
		"{ original =  " ^ (Store.json_string_of_domain original) ^
		"; current = " ^ (Store.json_string_of_domain current) ^
		"}"


let record_eq : record -> record -> bool = fun a b ->
	let eq = Store.domain_eq in
	match a, b with
		| New a, New b -> eq a b
		| Saved a, Saved b -> eq a b
		| Modified { original = original_a; current = current_a },
		  Modified { original = original_b; current = current_b } ->
			eq original_a original_b && eq current_a current_b
		| New _, _ | Saved _, _ | Modified _, _ -> false

type state = {
	domain : record;
	db : Store.t;
	user : Client_auth.auth_state;
}

let string_of_state { domain; db = _; user } =
	"{ domain = " ^ (string_of_record domain) ^
	"; user = " ^ (Client_auth.string_of_auth_state user) ^
	"}"

let eq a b =
	let { user = user_a; domain = domain_a; db = db_a } = a
	and { user = user_b; domain = domain_b; db = db_b } = b in
	record_eq domain_a domain_b &&
	user_a = user_b &&
	Store.eq db_a db_b

let domain_text_of_record : record -> string = let open Store in function
	| New d -> d.domain
	| Saved d -> d.domain
	| Modified { current; _} -> current.domain

let current_of_record = function
	| New d | Saved d -> d
	| Modified { current; _ } -> current

let current_of_state state = current_of_record state.domain

type edit =
	| Length of int
	| Notes of string option
	| Suffix of string option

let string_of_edit = function
	| Length l -> "Length " ^ (string_of_int l)
	| Notes n -> "Notes " ^ (Option.to_string quote_string n)
	| Suffix s -> "Suffix " ^ (Option.to_string quote_string s)

type external_state = Store.t * Client_auth.auth_state
type message =
	| Delete
	| Save
	| Edit of edit
	| Domain_changed of string
	| External_state of external_state

let string_of_message = function
	| Delete -> "Delete"
	| Save -> "Save"
	| Edit edit -> "Edit (" ^ (string_of_edit edit) ^ ")"
	| Domain_changed d -> "Domain_changed " ^ (quote_string d)
	| External_state _ -> "External_state ( ... )"

let update_domain state (current : Store.domain) =
	let saved = Store.lookup current.Store.domain state.db in
	{ state with domain =
		match saved with
			| Some original when Store.domain_eq original current -> Saved original
			| Some original -> Modified { original; current }
			| None -> New current
	}

let pair a b = a,b
let external_state sync : external_state React.signal = let open Sync in S.l2 pair sync.db_fallback sync.auth_state
let external_messages state = [ External_state state ]

let initial (db, user) domain_text =
	{
		db; user; domain = New (Store.default db domain_text);
	}

let command ~sync =
	let modify_domain original current =
		let as_record = Option.map (fun d -> Store.Domain d) in
		let (_saved:bool) = Sync.save_change ~state:sync
			~original:(original |> as_record) (current |> as_record) in
		()
	in
	fun state message -> let () = match message with
		| Delete ->
			(match state.domain with
				| New _ -> ()
				| Modified { original; _ } | Saved original ->
					modify_domain (Some original) None
			)
		| Save ->
			(match state.domain with
				| Saved _ -> ()
				| Modified { original; current } ->
					modify_domain (Some original) (Some current)
				| New current ->
					modify_domain None (Some current)
			)
		| _ -> ()
	in None

let update state =
	(function
		| Domain_changed text ->
			{ state with domain = match Store.lookup text state.db with
				| Some domain -> Saved domain
				| None -> New (Store.default state.db text)
			}

		| Edit edit ->
			let current = current_of_state state in
			let open Store in
			let current = match edit with
				| Length length -> { current with length }
				| Notes note -> { current with note }
				| Suffix suffix -> { current with suffix }
			in
			update_domain state current

		| External_state (db, user) ->
			update_domain { state with db; user } (current_of_state state)

		(* These two are handled mainly by `change`, triggers `External_state` message.
		 * The exception is that we also clear out modifications when we delete
		 * a domain from the db *)
		| Save -> state
		| Delete ->
			let domain = New (
				Store.default state.db (domain_text_of_record state.domain)
			) in
			{ state with domain }
	)

let non_empty_string : string -> string option = Option.non_empty ~zero:""
let default_empty_string : string option -> string = Option.default ""

let view _instance =
	let domain_info_editor ~get ~set =
		let save = track_input_contents set in
		fun state ->
			input ~a:[
				a_input_type `Text;
				a_oninput save;
				a_value (state |> current_of_state |> get);
			] ()
		in

	let notes_input = domain_info_editor
		~get:(fun d -> d.Store.note |> default_empty_string)
		~set:(fun v -> Edit (Notes (non_empty_string v))) in

	let length_input = domain_info_editor
		~get:(fun d -> d.Store.length |> string_of_int)
		~set:(fun v -> Edit (Length (int_of_string v))) in

	let suffix_input = domain_info_editor
		~get:(fun d -> d.Store.suffix |> default_empty_string)
		~set:(fun v -> Edit (Suffix (non_empty_string v))) in

	fun state -> (
		let empty_domain = (domain_text_of_record state.domain) = "" in
		if empty_domain then empty else (
			let is_authenticated = Client_auth.confirmed_uid_of_state state.user |> Option.is_some in
			let changed_domain = match state.domain with
				| Saved _ -> false
				| New _ | Modified _ -> true
			in
			let known_domain = match state.domain with
				| Saved _ | Modified _ -> true
				| New _ -> false
			in
			let save_button =
				if is_authenticated && changed_domain then
					input ~a:[
						a_input_type `Button;
						a_value "save";
						a_title "(ctrl+s)";
						a_onclick (emitter Save);
					] ()
				else empty
			in

			let delete_button = if is_authenticated && known_domain
				then a ~a:[
					a_class "delete link";
					a_onclick (emitter Delete);
				] [icon "remove"]
				else empty
			in

			let inline_input text input =
				div ~a:[a_class "inline"] [
					strong [Html.text text];
					span [input];
				]
			in

			div ~a:[
				a_class_list (List.filter_map identity [
					Some "domain-info panel";
					if known_domain then None else Some "unknown";
				])
			] [
				div ~a:[a_class "panel-heading"] [
					h3 [
						delete_button;
						text (match state.domain with
							| Modified { current; _ } ->
								current.Store.domain ^ " *"
							| New current | Saved current -> current.Store.domain
						);
						save_button;
					];
				];
				div ~a:[a_class "panel-body"] [
					inline_input "Length:" (length_input state);
					inline_input "Notes:" (notes_input state);
					inline_input "Suffix:" (suffix_input state);
				];
			]
		)
	)

let global_listeners instance map_msg = [
		global_event_listener Dom_html.Event.click (fun e ->
			e |> Vdoml.Event.keyboard_event |> Option.may (fun e ->
				if (Js.to_bool e##.ctrlKey && Keycode.of_event e = Keycode.KeyS) then (
					Dom.preventDefault e;
					Dom_html.stopPropagation e;
					Ui.emit instance (map_msg Save)
				);
			)
		)
	]

let component = Ui.component ~view ~eq ()
