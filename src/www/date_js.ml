open Js_of_ocaml
open Passe
open Js
include Date_common
let time () : float =
	let date = new%js date_now in
	let ms = date##getTime in
	ms /. 1000.0
