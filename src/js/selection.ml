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

let is_fully_selected ~length node =
	let sel = doc##getSelection() in
	if sel##rangeCount == 1 then begin
		let range = sel##getRangeAt(0) in
		if (range##startContainer == node && range##endContainer == node)
		then (range##startOffset = 0 && range##endOffset = length)
		else false
	end else false

let select node =
	let sel = doc##getSelection() in
	let range = doc##createRange() in
	let node = (node :> #Dom.node Js.t) in
	range##selectNodeContents(node);
	sel##removeAllRanges();
	sel##addRange(range)

let deselect () =
	let sel = doc##getSelection() in
	sel##removeAllRanges()

