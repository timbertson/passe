type log_level =
	| Always
	| Error
	| Warn
	| Info
	| Debug
	| Trace

let all_levels = [ Error;Warn;Info;Debug;Trace]

let ord lvl =
	match lvl with
	| Always -> 100
	| Error -> 50
	| Warn  -> 40
	| Info  -> 30
	| Debug -> 20
	| Trace -> 10

let lvl_scale = 10

let string_of_level lvl =
	match lvl with
	| Always -> ""
	| Error -> "ERROR"
	| Warn  -> "WARNING"
	| Info  -> "INFO"
	| Debug -> "DEBUG"
	| Trace -> "TRACE"

let default_formatter name lvl =
	if lvl = Always
	then ("", "")
	else ( "[" ^ (string_of_level lvl) ^ ":" ^ name ^ "] ", "")

let current_formatter = ref default_formatter

let current_level = ref (ord Debug)

let current_writer = ref (fun dest str -> output_string dest str; flush dest)

let logf = fun name lvl ->
	if (ord lvl) >= !current_level then (
		let dest = IFDEF JS THEN if (ord lvl) > (ord Info) then stderr else stdout ELSE stderr END in
		let (pre, post) = !current_formatter name lvl in
		let write = !current_writer in
		let print str = write dest (pre ^ str ^ post ^ "\n") in
		Printf.ksprintf print
	) else
		(* XXX can we prevent formatting from happening? *)
		Printf.ksprintf ignore

class logger name = object (self)
	method log   : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Always
	method error : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Error
	method warn  : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Warn
	method info  : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Info
	method debug : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Debug
	method trace : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Trace
end

let get_logger name = new logger name
