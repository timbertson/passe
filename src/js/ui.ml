(* module StringMap = Map.Make(String) *)
(*  *)
(* type node = *)
(* 	| Text of string *)
(* 	| Elem of element *)
(*  *)
(* and element = { *)
(* 	tag: string; *)
(* 	attrs: StringMap.t; *)
(* 	children: node list; *)
(* 	(* mechanism: (Dom.element -> unit Lwt.t) list; *) *)
(* } *)

(* type content = *)
(* 	| Plain of Dom.element *)
(* 	| Active of (Dom.element, [(Dom.element -> unit Lwt.t) list]) *)

let withContent : #Dom.node Js.t -> #Dom.node Js.t ->
	(#Dom.node Js.t -> unit Lwt.t) -> unit Lwt.t =
	fun parent elem block ->
		Dom.appendChild parent elem;
		try_lwt
			block elem
		finally (
			Dom.removeChild parent elem;
			Lwt.return_unit
		)

 
