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
	let home_dir = try Some (Unix.getenv "HOME") with Not_found -> None
	let generate ~length env args =
		ignore (env.config.Config.field "test");
		(* let db = Option.bind home_dir (fun home -> *)
		(* 	load_store (Filename.concat home ".config/supergenpass/ogp.json") *)
		(* ) in *)
		(* let length = Opt.get length in *)
		let domain = match args with
			| [] ->
					prerr_string "Domain: ";
					flush stderr;
					input_line stdin
			| [d] -> d
			| _ -> raise @@ SafeError "too many arguments"
		in
		let password = "test" in
		let domain = Store.default domain in
		let generated = Password.generate ~domain password in
		print_endline ("generated password for " ^ domain.Store.domain ^ ": " ^ generated)
end

module Options =
struct
	open OptParse
	(* let update = StdOpt.store_true () *)
	let length = StdOpt.int_option ~default:10 ()
	(* let trace = StdOpt.store_true () *)
	(* let verbosity = ref Var.default_verbosity *)
	(* let quiet = StdOpt.decr_option   ~dest:verbosity () *)
	(* let verbose = StdOpt.incr_option ~dest:verbosity () *)
	(* let interactive = StdOpt.store_true () *)
	(* let dry_run = StdOpt.store_true () *)
	(* let force = StdOpt.store_true () *)
	(* let metadata = StdOpt.store_true () *)
	let action = ref (Actions.generate ~length)
	open OptParser

	let main () =
		let options = OptParser.make ~usage: ("Usage: ogp [OPTIONS] [domain]") () in
		add options ~short_name:'l' ~long_name:"length" ~help:"length of generated password" length;
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
