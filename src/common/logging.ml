(* common interface between js / native logger module *)

let get_logger name =
	IFDEF JS THEN
		new Logging_js.logger name
	ELSE
		new Logging_native.logger name
	END

