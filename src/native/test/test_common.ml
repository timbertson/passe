open Passe
module Json_ext = Json_ext
module Common = Common
include Passe_unix
include OUnit2
module J = Json_ext

(* module-wide setup *)
let (>::) desc test =
	let initial_reporter = Logs.reporter () in
	let setup ctx =
		let ppf, flush =
			let b = Buffer.create 255 in
			let flush () = let s = Buffer.contents b in Buffer.clear b; s in
			Format.formatter_of_buffer b, flush
		in

		let report_fn src level ~over k msgf =
			let l = match level with
				| Logs.Error -> `Error
				| Logs.Warning -> `Warning
				| Logs.App | Logs.Info | Logs.Debug -> `Info
			in
			let k _ =
				logf ctx l "%s" (flush ());
				over ();
				k ()
			in
			msgf @@ fun ?header ?tags fmt ->
			Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
		in
		Logs.(set_reporter { report = report_fn })


	in
	let teardown () _ =
		Logs.set_reporter initial_reporter
	in
	desc >:: (fun ctx ->
		bracket setup teardown ctx;
		test ctx
	)
