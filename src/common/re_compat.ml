(* cross-env abtraction over js Regexp / native Str *)

IFDEF JS THEN
	module Str = struct
		let regexp = Regexp.regexp
		let regexp_string = Regexp.regexp_string
		let string_match r s = Regexp.string_match r s 0 |> Option.is_some
		let contains r s = Regexp.search_forward r s 0 |> Option.is_some
	end
ELSE
	module Str = struct
		let regexp = Str.regexp
		let regexp_string = Str.regexp_string
		let string_match r s = Str.string_match r s 0
		let contains r s = try let (_:int) = Str.search_forward r s 0 in true with Not_found -> false
	end
END

