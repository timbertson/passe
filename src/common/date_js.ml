open Js
include Date_common
let time () : float =
	let date = jsnew date_now () in
	let ms = date##getTime() in
	ms /. 1000.0
