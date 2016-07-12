open Batteries
open Passe
open Common
open Lwt
module Header = Cohttp.Header
module Connection = Cohttp.Connection
module J = Json_ext

let cwd = Unix.getcwd ()

let abs p = if Filename.is_relative p
	then Filename.concat cwd p
	else p

module Logging = Logging.Make(Logging.Unix_output)
module HTTP = Cohttp_lwt_unix.Server

module Fs = struct
	include Passe_server.Filesystem.Make(FS_unix)(Passe_server.Filesystem_unix.Atomic)(Logging)
	(* XXX it'd be nice to use unwrap_lwt, but trying to constraint the Filesystem
	 * so that `error` = `Fs.error` runs afoul of something a lot like
	 * https://blogs.janestreet.com/using-with-type-on-a-variant-type/
	 *)
	let connect () = match_lwt FS_unix.connect "/" with
		| `Ok fs -> return fs
		| _ -> failwith "fs.connect() failed"
end

module Auth = Passe_server.Auth.Make(Logging)(Clock)(Passe_server.Hash_bcrypt)(Fs)
module Unix_server = Passe_server.Service.Make(Logging)(Fs)(HTTP)(Auth)(Re_native)
open Unix_server
module Version = Version.Make(Re_native)
let log = Logging.get_logger "service"

let start_server ~host ~port ~development ~document_root ~data_root () =
	let open Cohttp_lwt_unix in
	log#info "Listening on: %s %d" host port;
	let document_root = abs document_root
	and data_root = abs data_root in
	log#info "Document root: %s" document_root;
	log#info "Data root: %s" data_root;
	let enable_rc = try Unix.getenv "PASSE_TEST_CTL" = "1" with _ -> false in
	if enable_rc then log#warn "Remote control enabled (for test use only)";
	lwt fs = Fs.connect () in
	let user_db = make_db fs data_root in
	let conn_closed (_ch, _conn) = log#trace "connection closed" in
	let callback = Unix_server.handler
		~document_root
		~data_root:(ref data_root)
		~user_db:(ref user_db)
		~fs
		~enable_rc
		~development
	in
	lwt () = Nocrypto_entropy_lwt.initialize () in
	let config = HTTP.make ~callback ~conn_closed () in
	let mode = `TCP (`Port port) in
	lwt ctx = Conduit_lwt_unix.init ~src:host () in
	let ctx = Cohttp_lwt_unix_net.init ~ctx () in
	HTTP.create ~ctx ~mode config


let main () =
	let open Extlib in
	let open OptParse in
	let open OptParser in

	let program_root = Filename.dirname (Filename.dirname (Sys.executable_name)) in

	let port = StdOpt.int_option ~default:2055 () in
	let host = StdOpt.str_option ~default:"127.0.0.1" () in
	let document_root = StdOpt.str_option ~default:(Filename.concat program_root "share/www") () in
	let development = StdOpt.store_true () in
	let data_root = StdOpt.str_option ~default:"data" () in
	let show_version = StdOpt.store_true () in
	let default_verbosity = Logging.ord Logging.Info in
	let verbosity = ref 0 in
	let louder = StdOpt.decr_option ~dest:verbosity () in
	let quieter = StdOpt.incr_option ~dest:verbosity () in

	let options = OptParser.make ~usage: ("Usage: service [OPTIONS]") () in
	add options ~short_name:'p' ~long_name:"port" port;
	add options ~long_name:"host" host;
	add options ~long_name:"root" document_root;
	add options ~long_name:"data" data_root;
	add options ~long_name:"development" ~help:"disable appcache" development;
	add options ~long_name:"version" show_version;
	add options ~short_name:'v' ~long_name:"verbose" louder;
	add options ~short_name:'q' ~long_name:"quiet" quieter;
	let posargs = OptParse.OptParser.parse ~first:1 options Sys.argv in
	if List.length posargs <> 0 then (
		prerr_endline "Too many arguments";
		exit 1
	);
	let log_version (meth:('a, unit, string, unit) format4 -> 'a) =
		meth "passe version: %s" (Version.pretty ()) in
	if Opt.get show_version then begin
		log_version log#log;
		exit 0
	end;
	Logging.current_level := default_verbosity + (!verbosity * Logging.lvl_scale);
	let verbosity_desc = try Logging.all_levels
		|> List.find (fun l -> Logging.ord l = !Logging.current_level)
		|> Logging.string_of_level
		with Not_found -> string_of_int !Logging.current_level
	in
	log#log " ( Log level: %s )" verbosity_desc;
	log_version log#debug;
	let document_root = Opt.get document_root in
	let data_root = Opt.get data_root in
	let dbdir = user_db_dir data_root in
	let () = try Unix.mkdir dbdir 0o700
		with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
	in
	let host = match (Opt.get host) with
		| "any" -> "0.0.0.0"
		| h -> h
	in
	Lwt_unix.run (start_server
		~port:(Opt.get port)
		~host
		~development:(Opt.get development)
		~data_root
		~document_root
	())


let () = Printexc.pass main ()
