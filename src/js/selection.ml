open Js

class type range = object
	method selectNodeContents : #Dom.node -> unit meth
end

class type selection = object
	method removeAllRanges : unit meth
	method addRange : range t -> unit meth
end

class type doc = object
	method getSelection : selection t meth
	method createRange : range t meth
end

let select node =
	let doc:doc Js.t= Unsafe.variable "document" in
	let sel = doc##getSelection() in
	let range = doc##createRange() in
	range##selectNodeContents(node);
	sel##removeAllRanges();
	sel##addRange(range)
