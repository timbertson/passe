open Passe
module Header = Cohttp.Header
module Connection = Cohttp.Connection
module J = Json_ext
open Common
module Log = (val Logging.log_module "service")

module HTTP = Cohttp_lwt_unix.Server
module Fs = Filesystem.Make(FS_unix)(Filesystem_unix.Atomic)

module Kv_fs = Kv_store.Of_fs(Fs)
module Version = Version.Make(Passe_unix.Re)
module Timed_log = Timed_log.Make(Pclock)


type data_source = [
	| `Cloud_datastore of string
	| `Fs of string
]

let start_server ~host ~port ~development ~document_root ~data_source () =
	let (data_module, data_root) =
		match data_source with
			| `Cloud_datastore url -> (module Cloud_datastore: Kv_store.Sig), url
			| `Fs root -> (module Kv_fs : Kv_store.Sig), root
	in

	let module Data = (val data_module) in
	let module Auth = Auth.Make(Pclock)(Hash_bcrypt)(Data) in
	let module Static_files = Static.Store(Kv_fs) in
	let module Unix_server = Service.Make(Version)(Pclock)(Data)(Static_files)(HTTP)(Server_config_unix)(Auth)(Passe_unix.Re) in

	Log.info (fun m->m "Listening on: %s %d" host port);
	Log.info (fun m->m "Document root: %s" document_root);
	Log.info (fun m->m "Data root: %s" data_root);
	let static_store = Static_files.init (Kv_fs.connect document_root) in
	let make_data_kv root = Data.connect (root |> Option.default data_root) in

	let%lwt clock = Pclock.connect () in
	let enable_rc = try Unix.getenv "PASSE_TEST_CTL" = "1" with _ -> false in
	if enable_rc then Log.warn (fun m->m "Remote control enabled (for test use only)");

	let conn_closed (_ch, _conn) = Log.debug (fun m->m "connection closed") in
	let callback = Unix_server.handler
		~static:static_store
		~make_data_kv
		~clock
		~enable_rc
		~development
	in
	let%lwt () = Nocrypto_entropy_lwt.initialize () in
	let config = HTTP.make ~callback ~conn_closed () in
	let mode = `TCP (`Port port) in
	let%lwt ctx = Conduit_lwt_unix.init ~src:host () in
	let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
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

	let cloud_datastore = ref None in
	let cloud_datastore_opt = StdOpt.str_callback (fun s -> cloud_datastore := Some s) in

	let options = OptParser.make ~usage: ("Usage: service [OPTIONS]") () in
	add options ~short_name:'p' ~long_name:"port" port;
	add options ~long_name:"host" host;
	add options ~long_name:"root" document_root;
	add options ~long_name:"data" data_root;
	add options ~long_name:"development" ~help:"disable appcache" development;
	add options ~long_name:"version" show_version;
	add options ~long_name:"timestamp" timestamp;
	add options ~long_name:"cloud-datastore" cloud_datastore_opt;
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

	let abs p = if Filename.is_relative p
		then Filename.concat (Unix.getcwd ()) p
		else p
	in
	let document_root = abs (Opt.get document_root) in
	let data_source = match !cloud_datastore with
		| Some url -> `Cloud_datastore url
		| None -> `Fs (abs (Opt.get data_root))
	in

	let host = match (Opt.get host) with
		| "any" -> "0.0.0.0"
		| h -> h
	in

	Lwt_main.run (start_server
		~port:(Opt.get port)
		~host
		~development:(Opt.get development)
		~data_source
		~document_root
	())

