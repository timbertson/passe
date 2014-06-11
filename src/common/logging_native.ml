open Logging_common

class logger name = object (self)
	method log s = print_endline s

	(* printf-style versions *)
	method error : 'a. ('a, out_channel, unit) format -> 'a = logf name Error
	method warn  : 'a. ('a, out_channel, unit) format -> 'a = logf name Warn
	method info  : 'a. ('a, out_channel, unit) format -> 'a = logf name Info
	method debug : 'a. ('a, out_channel, unit) format -> 'a = logf name Debug
	method trace : 'a. ('a, out_channel, unit) format -> 'a = logf name Trace
end
