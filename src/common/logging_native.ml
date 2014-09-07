open Logging_common

class logger name = object (self)
	method log s = print_endline s

	(* printf-style versions *)
	method error : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Error
	method warn  : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Warn
	method info  : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Info
	method debug : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Debug
	method trace : 'a. ('a, unit, string, unit) format4 -> 'a = logf name Trace
end
