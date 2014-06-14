open Store

let rec foldi i f acc =
	if i <= 0 then acc else foldi (pred i) f (f acc)

let sgp_alphabet = [|
	'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
	'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
	'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
	'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'9';'8'
|]

	(* XXX string encoding! *)
let generate ~domain:domain password =
	let generated = (domain.domain^":"^password) |> foldi 10 (fun input ->
		log#info "input: %s" input;
		let result = Digest.string input
		|> Base64.encode ~tbl:sgp_alphabet in
		log#info "result: %s" result;
		result
	) in
	generated

