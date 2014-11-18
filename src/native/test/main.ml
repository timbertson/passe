open Passe
open OUnit2

let log = Logging.get_logger "test"

(* let finally_do : 'i 'o. (unit -> unit) -> ('i -> 'o) -> 'i -> 'o = fun cleanup fn res -> *)
(* 	let rv = try *)
(* 		fn res *)
(* 	with e -> (cleanup (); raise e) in *)
(* 	log#info "action ok, cleaning"; *)
(* 	cleanup (); *)
(* 	rv *)

let mkdtemp () =
	let d = Filename.temp_file "passe-test-" ".tmp" in
	Unix.unlink d;
	Unix.mkdir d 0o755;
	d

let port_ready port =
	let open Unix in
	let fd = socket PF_INET SOCK_STREAM 0 in
	let rv = try
		connect fd (ADDR_INET (inet_addr_loopback, port));
		true
	with Unix_error(_,_,_) as _e -> (
		(* log#info "caught: %s" (Printexc.to_string _e); *)
		false
	) in
	Unix.close fd;
	rv


let await_tcp ~port ~pid =
	while (
		if port_ready port then false else (
			let open Unix in
			let dead_pid, (_:process_status) = waitpid [WNOHANG] pid in
			if dead_pid = pid then failwith "Child process died";
			true
		)
	) do
		log#info "Awaiting port %d ..." port;
		Unix.sleep 1
	done

let () =
	Logging.current_level := Logging.ord Logging.Debug;
	Printexc.record_backtrace true;
	Unix.putenv "PASSE_TEST_CTL" "1";

	let server = ref None in
	let temp_root = mkdtemp () in
	let cleanup () =
		log#info "deleting %s" temp_root;
		let open Unix in
		let kill pid =
			let signal = ref Sys.sigterm in
			let status : (int * process_status) option ref = ref None in
			log#info "Killing pid %d" pid;
			kill pid !signal;
			Lwt_main.run (while_lwt (Option.is_none !status) do
				(* on each loop iteration, either sleep for 3 seconds and kill, or
				 * wait for the proc to end *)
				log#debug "Waiting for pid %d" pid;
				let open Lwt in
				pick [
					(
						lwt () = Lwt_unix.sleep 3.0 in
						log#info "re-killing...";
						kill pid !signal;
						return_unit
					);
					(lwt s = Lwt_unix.waitpid [] pid in status := Some s; return_unit)
				]
			done);
			log#debug "pid %d exited";
		in
		!server |> Option.may kill;
		server := None;

		if try Unix.getenv "PASSE_TEST_LEAVE_TMP" = "1" with Not_found -> false then (
			let (_, status) = create_process "rm" [| "rm"; "-rf"; temp_root; |] stdin stdout stderr |> waitpid [] in
			if status <> (WEXITED 0) then failwith "rm -rf failed"
		)
	in

	(* can't use try/catch, because ounit probably calls exit() directly *)
	at_exit cleanup;

	let test_port = 8119 in

	if port_ready test_port then
		log#info "Using existing server on port %d" test_port
	else begin
		let server_pid = (
			let here = Filename.dirname Sys.argv.(0) in
			let path = Filename.concat here "../../../tools/passe-server" in
			let open Unix in

			let cmd = [| path; "-qq"; "--port"; string_of_int test_port|] in
			log#info "Running: `%s`" (String.concat " " (Array.to_list cmd));
			create_process cmd.(0) cmd stdin stdout stderr
		) in
		log#info "launched pid %d" server_pid;
		server := Some server_pid;
		await_tcp ~port:test_port ~pid:server_pid;
	end;

	(* inject test setup... *)
	Server.root_url := "http://localhost:"^(string_of_int test_port)^"/" |> Uri.of_string;

	let () = match Server.post_json
		~data:(`Assoc [("data",`String temp_root)])
		(Server.path ["ctl"; "init"]) |> Lwt_main.run
	with
		| Server.OK _ -> ()
		| _ -> failwith "/ctl/init failed"
	in

	run_test_tt_main (OUnit2.test_list [
		Sync_tests.suite
	])
