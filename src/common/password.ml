open Common

module Make (Re:Re_ext.Sig)(Logging:Logging.Sig) = struct

	let log = Logging.get_logger "password"
	module Store = Store.Make(Re)(Logging)
	open Store

	let rec foldi i f acc =
		if i <= 0 then acc else foldi (pred i) f (f acc)

	let rec fold_until pred f acc =
		if (pred acc) then acc
		else fold_until pred f (f acc)

	let sgp_alphabet = [|
		'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
		'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
		'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
		'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'9';'8'
	|]

	let upper = Re.regexp "[A-Z]"
	let digit = Re.regexp "[0-9]"
	let leading_lower = Re.regexp "^[a-z]"

	let valid_password p =
		let has r = Re.contains r p in
		has leading_lower && has upper && has digit

		(* XXX tests with non-ascii string encoding! *)
	let generate ~domain:domain password =
		let count = ref 0 in
		let iter = (fun input ->
			count := succ !count;
			(* log#info "input: %s (%d)" input !count; *)
			let result = Digest.string input
			|> Base64.encode ~tbl:sgp_alphabet ~pad:'A' in
			(* log#info "result: %s" result; *)
			log#debug "hashed -> %s" result;
			result
		) in

		let trim p =
			String.sub p 0 domain.length in

		let password = match domain.suffix with
			| Some suff -> password^suff
			| None -> password
		in
		let input = (password^":"^domain.domain) in
		log#debug "password input: %s" input;
		let generated = input
			|> foldi 10 iter
			|> fold_until (valid_password % trim) iter
			|> trim
		in
		generated

end
