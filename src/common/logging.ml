let default_verbosity = 1 (* Warn *)

let tagging_reporter reporter =
	{ Logs.report = (fun src level ~over k user_msgf ->
		if (Logs.Src.equal src Logs.default || level = Logs.App) then
			reporter.Logs.report src level ~over k user_msgf
		else
			reporter.Logs.report src level ~over k (fun outer_msgf ->
				user_msgf (fun ?header ?tags fmt ->
					outer_msgf ?header ?tags ("[%a %s] @[" ^^ fmt ^^ "@]")
						Logs.pp_level level
						(Logs.Src.name src)
				)
		)
	)}

let set_reporter reporter = Logs.set_reporter (tagging_reporter reporter)

let () =
	(* initialize with default reporter *)
	let pp_header fmt (lvl, src) = () in
	set_reporter (Logs.format_reporter ~pp_header ())

let apply_verbosity verbosity =
	let open Logs in
	(* enable backtraces if at least one -v is given *)
	if verbosity > 1 then Printexc.record_backtrace true;
	let log_level = match verbosity with
		| 1 -> Warning
		| 2 -> Info
		| n -> if n <= 0 then Error else Debug
	in
	set_level ~all:true (Some log_level);
	log_level

let log_module name = Logs.src_log (Logs.Src.create name)
