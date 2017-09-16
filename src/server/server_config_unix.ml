let is_sandstorm () =
	(try Unix.getenv "SANDSTORM" = "1" with Not_found -> false)
