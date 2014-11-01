exception SafeError of string

let print_exc dest exc =
	output_string dest (Printexc.to_string exc)

open Passe
open Batteries
open Extlib
module Json = Yojson.Safe

let log = Logging.get_logger "passe"

type env = {
	config : Config.t;
}

let too_many_args () = raise @@ SafeError "too many arguments"

module Actions = struct
	open OptParse
	let generate ~use_clipboard ~quiet env args =
		let domain = match args with
			| [] -> None
			| [d] -> Some d
			| _ -> too_many_args ()
		in
		Lwt_main.run (Ui.main ~domain ~quiet ~use_clipboard ~config:(env.config) ())

	let sync env args =
		let () = match args with
			| [] -> ()
			| _ -> too_many_args ()
		in
		Lwt_main.run (Ui.sync_ui (Sync.build env.config))

	let list_domains env args =
		let domain = match args with
			| [] -> None
			| [domain] -> Some domain
			| _ -> too_many_args ()
		in
		let ok = Ui.list_domains env.config domain in
		exit (if ok then 0 else 1)
end

let apply_verbosity verbosity =
	let open Logging in
	(* enable backtraces if at least one -v is given *)
	if verbosity > 1 then Printexc.record_backtrace true;
	let new_level = (match verbosity with
		| 1 -> Warn
		| 2 -> Info
		| 3 -> Debug
		| n -> if n <= 0 then Error else Trace
	) in
	Logging.current_level := Logging.ord new_level;
	log#info "Log level: %s" (Logging.string_of_level new_level)

let default_verbosity = 1 (* warnings only *)

module Options =
struct
	open OptParse
	(* let update = StdOpt.store_true () *)
	let use_clipboard = StdOpt.store_false ()
	let sync = StdOpt.store_true ()
	(* let trace = StdOpt.store_true () *)
	let verbosity = ref default_verbosity
	let quiet = StdOpt.decr_option   ~dest:verbosity ()
	let verbose = StdOpt.incr_option ~dest:verbosity ()
	let list_only = StdOpt.store_true ()
	(* let interactive = StdOpt.store_true () *)
	(* let dry_run = StdOpt.store_true () *)
	(* let force = StdOpt.store_true () *)
	(* let metadata = StdOpt.store_true () *)
	let action env posargs =
		apply_verbosity !verbosity;
		if Opt.get sync then
			Actions.sync env posargs
		else if Opt.get list_only then
			Actions.list_domains env posargs
		else
			(* XXX remove `quiet` argument *)
			Actions.generate ~use_clipboard:(Opt.get use_clipboard) ~quiet:(!verbosity <=0) env posargs

	open OptParser

	let main () =
		let options = OptParser.make ~usage: ("Usage: passe [OPTIONS] [domain]") () in
		add options ~short_name:'p' ~long_name:"plain" ~help:"print password (don't copy to clipboard)" use_clipboard;
		add options ~short_name:'q' ~long_name:"quiet" quiet;
		add options ~short_name:'v' ~long_name:"verbose" verbose;
		add options ~short_name:'l' ~long_name:"list" list_only;
		add options ~long_name:"sync" sync;
		options
	;;
end

let getenv name = try Some (Unix.getenv name) with Not_found -> None

let main () =
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
	let env = {
		config = Config.build storage_provider;
	} in
	Options.action env posargs
