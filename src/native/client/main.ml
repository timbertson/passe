let print_exc dest exc =
	output_string dest (Printexc.to_string exc)

open Passe
open Passe_unix
open Batteries
open Extlib
open React_ext
module Json = Yojson.Safe
open Common

module Log = (val Logging.log_module "passe")

let too_many_args () = raise @@ SafeError "too many arguments"

module Actions = struct
	open OptParse
	let generate ~use_clipboard ~edit ~quiet env args =
		let domain = match args with
			| [] -> None
			| [d] -> Some d
			| _ -> too_many_args ()
		in
		Lwt_main.run (Ui.main ~domain ~edit ~quiet ~use_clipboard ~env ())

	let sync env args =
		let () = match args with
			| [] -> ()
			| _ -> too_many_args ()
		in
		Lwt_main.run (Ui.sync_ui (Sync.build env))

	let list_domains env args =
		let domain = match args with
			| [] -> None
			| [domain] -> Some domain
			| _ -> too_many_args ()
		in
		let ok = Ui.list_domains env domain in
		exit (if ok then 0 else 1)
	
	type config_field = {
		key : string;
		get : (Store.t signal -> string);
		change : (string -> Store.default_change);
	}
	
	let config env args =
		let state = Sync.build env in
		let open Store in
		match S.value (state.Sync.db_signal) with
		| None -> raise @@ SafeError "No current user; try --sync first"
		| Some db -> begin
			let db = state.Sync.db_fallback in
			let get_defaults db = Store.get_defaults (S.value db) in
			let fields = [{
				key = "length";
				get = (fun db -> (get_defaults db).default_length |> string_of_int);
				change = (fun len -> `Length (int_of_string len));
			}] in
			let lookup key =
				try List.find (fun f -> f.key = key) fields
				with Not_found -> raise @@ SafeError ("Unknown config key: "^key)
			in
			let print_field field = Log.app (fun m->m "%s: %s" (field.key) (field.get db)) in

			match args with
				| [] ->
					Log.app (fun m->m "# Current config for %s:" (state.Sync.current_uid |> S.value |> Option.get));
					fields |> List.iter print_field

				| [key] ->
					let field = lookup key in
					Log.app (fun m->m "%s" (field.get db))

				| [key; value] ->
					let field = lookup key in
					let change = field.change value in
					if not (Sync.save_default ~state change) then exit 1;
					print_field field

				| _ -> too_many_args ()
			end
end

module Options =
struct
	open OptParse
	let use_clipboard = StdOpt.store_false ()
	let sync = StdOpt.store_true ()
	let verbosity = ref Logging.default_verbosity
	let quiet = StdOpt.decr_option   ~dest:verbosity ()
	let verbose = StdOpt.incr_option ~dest:verbosity ()
	let list_only = StdOpt.store_true ()
	let config = StdOpt.store_true ()
	let edit = StdOpt.store_true ()
	let action env posargs =
		let log_level = Logging.apply_verbosity !verbosity in
		Logs.(info (fun m -> m "Log level: %a" pp_level log_level));

		if Opt.get sync then
			Actions.sync env posargs
		else if Opt.get list_only then
			Actions.list_domains env posargs
		else if Opt.get config then
			Actions.config env posargs
		else
			(* XXX remove `quiet` argument *)
			Actions.generate ~use_clipboard:(Opt.get use_clipboard) ~quiet:(!verbosity <=0) ~edit:(Opt.get edit) env posargs

	open OptParser

	let main () =
		let options = OptParser.make ~usage: ("Usage: passe [OPTIONS] [domain]") () in
		add options ~short_name:'p' ~long_name:"plain" ~help:"print password (don't copy to clipboard)" use_clipboard;
		add options ~short_name:'q' ~long_name:"quiet" quiet;
		add options ~short_name:'v' ~long_name:"verbose" verbose;
		add options ~short_name:'l' ~long_name:"list" list_only;
		add options ~long_name:"config" config;
		add options ~short_name:'e' ~long_name:"edit" edit;
		add options ~long_name:"sync" sync;
		options
	;;
end

let getenv name = try Some (Unix.getenv name) with Not_found -> None

let main () =
	try
		let p = Options.main () in
		let posargs = OptParse.OptParser.parse ~first:1 p Sys.argv in
		let storage_provider =
			let config_dir = match getenv "XDG_CONFIG_HOME" with
				| Some conf -> conf
				| None ->
					let conf = getenv "HOME" |> Option.map (fun home -> Filename.concat home ".config") in
					Option.get_exn conf (SafeError "neither $XDG_CONFIG_HOME or $HOME are defined")
			in
			let path = Filename.concat config_dir "passe/db.json" in
			new Config_storage.provider path
		in
		let env = Sync.{
			env_config_provider = Config.build storage_provider;
			env_initial_auth = `Explicit; (* TODO: support implicit in native client *)
		} in
		Options.action env posargs
	with SafeError e -> (prerr_endline e; exit 1)
