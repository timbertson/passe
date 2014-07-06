open Js
let time () : float =
	let date = jsnew date_now () in
	let ms = date##getTime() in
	ms /. 1000.0
