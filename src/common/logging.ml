(* common interface between js / native logger module *)
include Logging_common

let get_logger name =
	IFDEF JS THEN
		new Logging_js.logger name
	ELSE
		new Logging_native.logger name
	END

