(*
 * Base64 - Base64 codec
 * Copyright (C) 2003 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

exception Invalid_char
exception Invalid_table

external unsafe_char_of_int : int -> char = "%identity"

type encoding_table = char array
type decoding_table = int array

let chars = [|
	'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
	'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
	'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
	'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
|]

let string_of_char c = String.make 1 c

let encode ?(tbl=chars) input =
	let result = ref "" in
	if Array.length tbl <> 64 then raise Invalid_table;
	let data = ref 0 in
	let count = ref 0 in
	let flush() =
		if !count > 0 then begin
			let d = (!data lsl (6 - !count)) land 63 in
			result := !result ^ (string_of_char (Array.unsafe_get tbl d))
		end;
	in
	let write c =
		let c = int_of_char c in
		data := (!data lsl 8) lor c;
		count := !count + 8;
		while !count >= 6 do
			count := !count - 6;
			let d = (!data asr !count) land 63 in
			result := !result ^ (string_of_char (Array.unsafe_get tbl d))
		done;
	in
	(* XXX why len - 1? *)
	String.iter write input;
	flush();
	!result
