open Passe
module Header = Cohttp.Header
module Connection = Cohttp.Connection
module J = Json_ext
open Common

let cwd = Unix.getcwd ()

let abs p = if Filename.is_relative p
	then Filename.concat cwd p
	else p

module HTTP = Cohttp_lwt_unix.Server

module Fs = struct
	include Filesystem.Make(FS_unix)(Filesystem_unix.Atomic)
	let connect () = FS_unix.connect "/"
end

module Auth = Auth.Make(Pclock)(Hash_bcrypt)(Fs)
module Static_files = Static.Fs(Fs)
module Version = Version.Make(Passe_unix.Re)
module Unix_server = Service.Make(Version)(Pclock)(Static_files)(Fs)(HTTP)(Server_config_unix)(Auth)(Passe_unix.Re)
open Unix_server
module Timed_log = Timed_log.Make(Pclock)
module Log = (val Logging.log_module "service")

let start_server ~host ~port ~development ~document_root ~data_root () =
	Log.info (fun m->m "Listening on: %s %d" host port);
	let document_root = abs document_root
	and data_root = abs data_root in
	Log.info (fun m->m "Document root: %s" document_root);
	Log.info (fun m->m "Data root: %s" data_root);
	let document_root = Fs.Path.base document_root in

	let enable_rc = try Unix.getenv "PASSE_TEST_CTL" = "1" with _ -> false in
	if enable_rc then Log.warn (fun m->m "Remote control enabled (for test use only)");
	let%lwt fs = Fs.connect () in
	let%lwt clock = Pclock.connect () in

	let dbdir = db_path_for ?user:None (Fs.Path.base data_root)
		|> R.assert_ok string_of_invalid_path in
	let%lwt () = Fs.mkdir fs dbdir |> Lwt.map (function
		| Ok () | Error `File_already_exists -> ()
		| Error e -> failwith (Printf.sprintf "Couldn't create dbdir (%s): %s"
			(Fs.Path.to_unix dbdir)
			(Fs.string_of_write_error e)
		)
	) in

	let conn_closed (_ch, _conn) = Log.debug (fun m->m "connection closed") in
	let static_files = Static_files.init ~fs document_root in
	let callback = Unix_server.handler
		~static:static_files
		~data_root:data_root
		~clock
		~fs
		~enable_rc
		~development
	in
	let%lwt () = Nocrypto_entropy_lwt.initialize () in
	let config = HTTP.make ~callback ~conn_closed () in
	let mode = `TCP (`Port port) in
	let%lwt ctx = Conduit_lwt_unix.init ~src:host () in
	let ctx = Cohttp_lwt_unix_net.init ~ctx () in
	HTTP.create ~ctx ~mode config


let main () =
	let open OptParse in
	let open OptParser in

	let program_root = Filename.dirname (Filename.dirname (Sys.executable_name)) in

	let port = StdOpt.int_option ~default:2055 () in
	let host = StdOpt.str_option ~default:"127.0.0.1" () in
	let document_root = StdOpt.str_option ~default:(Filename.concat program_root "share/passe-server") () in
	let development = StdOpt.store_true () in
	let data_root = StdOpt.str_option ~default:"data" () in
	let show_version = StdOpt.store_true () in
	let verbosity = ref 0 in
	let timestamp = StdOpt.store_true () in
	let louder = StdOpt.incr_option ~dest:verbosity () in
	let quieter = StdOpt.decr_option ~dest:verbosity () in

	let options = OptParser.make ~usage: ("Usage: service [OPTIONS]") () in
	add options ~short_name:'p' ~long_name:"port" port;
	add options ~long_name:"host" host;
	add options ~long_name:"root" document_root;
	add options ~long_name:"data" data_root;
	add options ~long_name:"development" ~help:"disable appcache" development;
	add options ~long_name:"version" show_version;
	add options ~long_name:"timestamp" timestamp;
	add options ~short_name:'v' ~long_name:"verbose" louder;
	add options ~short_name:'q' ~long_name:"quiet" quieter;
	let posargs = OptParse.OptParser.parse ~first:1 options Sys.argv in
	if List.length posargs <> 0 then (
		prerr_endline "Too many arguments";
		exit 1
	);

	let log_version m =
		let fmt = (Pervasives.format_of_string "passe version: %s") in
		(* header + tags needed for type inference *)
		m ?header:None ?tags:None fmt (Version.pretty ())
	in

	if Opt.get show_version then begin
		Logs.app log_version;
		exit 0
	end;
	if (Opt.get timestamp) then (
		let clock = Pclock.connect () |> Lwt_main.run in
		Logs.set_reporter @@
			(* TODO: this order seems backwards... *)
			Logging.tagging_reporter @@
			Timed_log.reporter ~clock @@
			Logging.default_reporter
	);
	
	let log_level = Logging.(apply_verbosity (default_verbosity + !verbosity)) in
	Logs.(app (fun m -> m " ( Log level: %a )" pp_level log_level));
	Logs.debug log_version;

	Lwt.async_exception_hook := (fun ex ->
		Logs.err (fun m->m "ERROR: Uncaught exception from LWT: %s" (Printexc.to_string ex))
	);

	let document_root = Opt.get document_root in
	let data_root = Opt.get data_root in

	let host = match (Opt.get host) with
		| "any" -> "0.0.0.0"
		| h -> h
	in
	Lwt_main.run (start_server
		~port:(Opt.get port)
		~host
		~development:(Opt.get development)
		~data_root
		~document_root
	())

