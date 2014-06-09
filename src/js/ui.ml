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
(* 	| Plain of Dom_html.element Js.t *)
(* 	| Active of (Dom_html.element Js.t * (Dom_html.element Js.t -> unit Lwt.t) list) *)

type 'a content =
	| Plain of 'a Js.t
	| Active of ('a Js.t * ('a Js.t -> unit Lwt.t) list)

let mechanism fn node =
	match node with
		| Plain node -> Active (node, [fn])
		| Active (node, mechanisms) -> Active (node, fn :: mechanisms)

let plain : 'a Js.t -> 'a content = fun e -> Plain e

let stop () = Lwt.wait () |> fst

let withContent : #Dom_html.element Js.t -> 'a content ->
	('a Js.t -> unit Lwt.t) -> unit Lwt.t =
	fun parent content block ->
		let (elem, mechanisms) = match content with
			| Plain elem -> (elem, [])
			| Active pair -> pair
		in
		Dom.appendChild parent elem;
		try_lwt
			Lwt.pick [
				(
					lwt () = Lwt.join (mechanisms |> List.map (fun f -> f elem)) in
					let sleeper, _ = Lwt.wait () in
					sleeper
				);
				block elem
			]
		finally (
			Dom.removeChild parent elem;
			Lwt.return_unit
		)

 
