let human_time_span_desc t =
	let minutes = t /. 60.0 in
	let num, units = (
		if (minutes > 60.0) then (
			let hours = minutes /. 60.0 in
			if (hours > 24.0) then (
				let days = (hours /. 24.0) in
				days, "day"
			) else (
				hours, "hour"
			)
		) else (
			minutes, "minute"
		)
	) in
	let num = int_of_float num in
	let suff = if num = 1 then "" else "s" in
	(string_of_int num) ^ " " ^ units ^ suff


