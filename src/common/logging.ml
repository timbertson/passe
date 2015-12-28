module Static = struct
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

	let set_level lvl = current_level := ord lvl

	let current_writer = ref (fun dest str -> output_string dest str; flush dest)
end

module type Sig = sig
	include module type of Static
	class logger: string -> object
		method log   : 'a. ('a, unit, string, unit) format4 -> 'a
		method error : 'a. ('a, unit, string, unit) format4 -> 'a
		method warn  : 'a. ('a, unit, string, unit) format4 -> 'a
		method info  : 'a. ('a, unit, string, unit) format4 -> 'a
		method debug : 'a. ('a, unit, string, unit) format4 -> 'a
		method trace : 'a. ('a, unit, string, unit) format4 -> 'a
	end

	val get_logger : string -> logger
end

module type OUTPUT = sig
	val dest : Static.log_level -> out_channel
end

module Js_output = struct
	open Static
	let dest = function
		| Error
		| Warn
			-> stderr
		| Always
		| Info
		| Debug
		| Trace
			-> stdout
end

module Unix_output = struct
	open Static
	let dest (_:log_level) = stderr
end


module Make(Output:OUTPUT) = struct
	include Static

	let logf = fun name lvl ->
		if (ord lvl) >= !current_level then (
			let (pre, post) = !current_formatter name lvl in
			let write = !current_writer in
			let print str = write (Output.dest lvl) (pre ^ str ^ post ^ "\n") in
			Printf.ksprintf print
		) else
			(* XXX can we prevent formatting from happening? *)
			Printf.ksprintf ignore

	class logger name = object (_self)
		method log   : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Always
		method error : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Error
		method warn  : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Warn
		method info  : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Info
		method debug : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Debug
		method trace : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Trace
	end

	let get_logger name = new logger name
end
