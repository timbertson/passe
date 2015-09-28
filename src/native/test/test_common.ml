open Passe
include OUnit2
module J = Json_ext
module Logging = Logging.Make(Logging.Unix_output)

(* module-wide setup *)
let (>::) desc test =
	let initial_writer = !Logging.current_writer in
	let setup ctx =
		Logging.current_writer := (fun _dest str -> logf ctx `Info "%s" str)
	in
	let teardown () _ =
		Logging.current_writer := initial_writer
	in
	desc >:: (fun ctx ->
		bracket setup teardown ctx;
		test ctx
	)
