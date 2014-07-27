(* type options = { *)
(* 	length of int; *)
(* 	domain of string; *)
(* } *)

exception SafeError of string

let print_exc dest exc =
	output_string dest (Printexc.to_string exc)

open Batteries
open Extlib
module Json = Yojson.Safe

let log = Logging.get_logger "ogp"

type env = {
	config : Config.t;
}

module Actions = struct
	open OptParse
	let generate ~length ~use_clipboard ~quiet _env args =
		let domain = match args with
			| [] -> None
			| [d] -> Some d
			| _ -> raise @@ SafeError "too many arguments"
		in
		Lwt_main.run (Ui.main
			~domain
			~length:(Opt.get length)
			~quiet:(Opt.get quiet)
			~use_clipboard:(Opt.get use_clipboard)
			())
end

module Options =
struct
	open OptParse
	(* let update = StdOpt.store_true () *)
	let length = StdOpt.int_option ~default:10 ()
	let use_clipboard = StdOpt.store_false ()
	let quiet = StdOpt.store_true ()
	(* let trace = StdOpt.store_true () *)
	(* let verbosity = ref Var.default_verbosity *)
	(* let quiet = StdOpt.decr_option   ~dest:verbosity () *)
	(* let verbose = StdOpt.incr_option ~dest:verbosity () *)
	(* let interactive = StdOpt.store_true () *)
	(* let dry_run = StdOpt.store_true () *)
	(* let force = StdOpt.store_true () *)
	(* let metadata = StdOpt.store_true () *)
	let action = ref (Actions.generate ~length ~use_clipboard ~quiet)
	open OptParser

	let main () =
		let options = OptParser.make ~usage: ("Usage: ogp [OPTIONS] [domain]") () in
		add options ~short_name:'l' ~long_name:"length" ~help:"length of generated password" length;
		add options ~short_name:'p' ~long_name:"plain" ~help:"print password (don't copy to clipboard)" use_clipboard;
		add options ~short_name:'q' ~long_name:"quiet" quiet;
		options
	;;
end

let getenv name = try Some (Unix.getenv name) with Not_found -> None

let () =
	let p = Options.main () in
	let posargs = OptParse.OptParser.parse ~first:1 p Sys.argv in
	let storage_provider =
		let config_dir = match getenv "XDG_CONFIG_HOME" with
			| Some conf -> conf
			| None ->
				let conf = getenv "HOME" |> Option.map (fun home -> Filename.concat home ".config") in
				Option.get_exn conf (SafeError "neither $XDG_CONFIG_HOME or $HOME are defined")
		in
		let path = Filename.concat config_dir "ogp/db.json" in
		new Config_storage.provider path
	in
	let env = {
		config = Config.build storage_provider;
	} in
	!Options.action env posargs
