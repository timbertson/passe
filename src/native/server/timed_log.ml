module Make(Clock:Mirage_types.PCLOCK) = struct
	let fmt_timestamp time =
		let ptime = Ptime.v time in
		let (year,month,day),((hour,minute,second), _tz) = Ptime.to_date_time ptime in
		Printf.sprintf "%04d-%02d-%02d %02d:%02d.%02d"
			year month day hour minute second

	let reporter ~clock parent =
		Logs.({
			report = (fun src level ~over k user_msgf ->
				let now = Clock.now_d_ps clock in
				parent.report src level ~over k (fun outer_msgf ->
					user_msgf (fun ?header ?tags fmt ->
						outer_msgf ?header ?tags ("%s @[" ^^ fmt ^^ "@]") (fmt_timestamp now)
					)
				)
			)
		})
end
