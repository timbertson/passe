module Make(C:V1.CLOCK) = struct
	let fmt_timestamp x =
		let open C in
		let tm = gmtime x in
		Printf.sprintf "%04d-%02d-%02d %02d:%02d.%02d"
		(tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

	let reporter parent =
		{ Logs.report = (fun src level ~over k user_msgf ->
			let now = C.time () in
			parent.Logs.report src level ~over k (fun outer_msgf ->
				user_msgf (fun ?header ?tags fmt ->
					outer_msgf ?header ?tags ("%s @[" ^^ fmt ^^ "@]") (fmt_timestamp now)
				)
			)
		)}
end
