open Common
open Lwt
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

(* type 'a content = *)
(* 	| Plain of 'a Js.t *)
(* 	| Active of ('a Js.t * ('a Js.t -> unit Lwt.t) list) *)
(*  *)
class type fragment = object
	method attach : #Dom.node Js.t -> unit Lwt.t
end

class type ['a] widget = object
	method attach_widget : #Dom.node Js.t -> ('a Js.t * unit Lwt.t)
end

class ['a] element ?(mechanisms=[]) (cons:unit -> #Dom.node Js.t) (children:#fragment list) =
object (self)
	method attach_widget : 'p. (#Dom.node as 'p) Js.t -> ('a Js.t * unit Lwt.t) = fun parent ->
		let elem: 'a Js.t = cons () in
		let attach_child : #fragment -> unit Lwt.t = fun frag -> frag#attach elem in
		let child_threads = children |> List.map attach_child in
		let mechanism_threads = mechanisms |> List.map (fun mech -> mech elem) in
		Dom.appendChild parent elem;
		(
			elem,
			try_lwt
				Console.log "attach_widget starting";
				Lwt.join @@ List.concat [
					child_threads;
					mechanism_threads
				]
			with e -> (
				Console.log "attach_widget caught ex";
				raise e
			)
			finally (
				Dom.removeChild parent elem;
				Lwt.return_unit
			)
		)
	
	method attach: 'p. (#Dom.node as 'p) Js.t -> unit Lwt.t = fun parent ->
		snd @@ self#attach_widget parent

	method private clone ?mechanisms:mechs ?children:cs () : 'a element =
		let mechanisms:('a Js.t -> unit Lwt.t) list = Option.default mechanisms mechs in
		let children = Option.default children cs in
		new element ~mechanisms:mechanisms cons children
	method mechanism mech = self#clone ~mechanisms:(mech :: mechanisms) ()
	method append (child:fragment) = self#clone ~children:(List.append children [child]) ()
	method append_all (new_children:fragment list) = self#clone ~children:(List.append children new_children) ()
	method prepend (child:fragment) = self#clone ~children:(child :: children) ()
end

let non_null o = Js.Opt.get o (fun () -> raise (AssertionError "unexpected null"))

let stream_mechanism s = fun elem ->
	let current:(Dom.node Js.t ref) = ref elem in
	let parent = ref None in
	s |> Lwt_stream.iter (fun new_val ->
		try
			Console.console##log_2 (Js.string("got new stream value: "), new_val);
			let p = (match !parent with
				| Some p -> p
				| None ->
						let p = (!current##parentNode) |> non_null in
						parent := Some p;
						p
			) in
			Console.log "loop next...";
			Dom.replaceChild p !current new_val;
			Console.log "loop next...";
			current := new_val;
			Console.log "loop next..."
		with e -> Console.log "YO, WTF?"; raise e
	)

let create_blank_node () =
	let elem = Dom_html.document##createComment(Js.string "placeholder") in
	(elem:>Dom.node Js.t)

let stream s = new element ~mechanisms:[stream_mechanism s] create_blank_node []

let textArea doc = new element (fun () -> Dom_html.createTextarea doc) []
let div ~children doc = new element (fun () -> Dom_html.createDiv doc) children

(* let mechanism fn node = *)
(* 	match node with *)
(* 		| Plain node -> Active (node, [fn]) *)
(* 		| Active (node, mechanisms) -> Active (node, fn :: mechanisms) *)
(*  *)
(* let plain : 'a Js.t -> 'a content = fun e -> Plain e *)
(*  *)
let pause () = Lwt.wait () |> fst

let withContent : #Dom.node Js.t -> 'w widget -> ('w Js.t -> unit Lwt.t) -> unit Lwt.t =
	fun parent content block ->
		let ((elem:'w Js.t), block_mech) = content#attach_widget parent in
		try_lwt
			Lwt.pick [
				(try_lwt
					Console.log "MECH START";
					lwt () = block_mech in
					Console.log "MECH DONE";
					Lwt.return_unit
				with e -> (
					Console.log "MECH caught";
					raise e
				)
				finally (
					Console.log("Mechanism ENDED")
				);
				pause ()
				);
				block elem
			]
		finally (
			Dom.removeChild parent (elem:>Dom.node Js.t);
			Lwt.return_unit
		)

 
