open Js_of_ocaml
open Js

class type range = object
	method selectNodeContents : #Dom.node t -> unit meth
	method commonAncestorContainer : #Dom.node t prop
	method startContainer : #Dom.node t prop
	method endContainer : #Dom.node t prop
	method startOffset : int prop
	method endOffset : int prop
end

class type selection = object
	method removeAllRanges : unit meth
	method addRange : range t -> unit meth
	method rangeCount : int prop
	method getRangeAt : int -> range t meth
end

class type doc = object
	method getSelection : selection t meth
	method createRange : range t meth
end

let doc:doc Js.t= Unsafe.variable "document"

let is_fully_selected ?length node =
	let length = match length with
		| Some l -> l
		| None ->
			let contents = node##.textContent in
			let length : int Js.Opt.t = Js.Opt.map contents (fun text -> text##.length) in
			Js.Opt.get length (fun () -> 0)
	in
	let sel = doc##getSelection in
	if sel##.rangeCount == 1 then begin
		let range = sel##getRangeAt 0 in
		if (range##.startContainer == node && range##.endContainer == node)
		then (range##.startOffset = 0 && range##.endOffset = length)
		else false
	end else false

let deselect () =
	doc##getSelection##removeAllRanges

let select node =
	deselect ();
	let sel = doc##getSelection in
	let range = doc##createRange in
	let node = (node :> #Dom.node Js.t) in
	range##selectNodeContents node;
	sel##addRange range

