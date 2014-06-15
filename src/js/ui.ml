open Common
open Lwt
open Lwt_react
let log = Logging.get_logger "ui"

(* FIXME: only required because raised exceptions don't seem to propagate properly
 * between #attach and withContent *)
type mechanism_result =
	| Complete
	| Error of exn

let pause () = Lwt.wait () |> fst

(* pause forever upon successful completion,
 * but exit early on error *)
let only_fail t =
	lwt result = t in
	match result with
		| Complete -> pause ()
		| Error _ as err -> return err

let pick_failed threads = Lwt.pick (threads |> List.map only_fail)

let convert_exception : unit Lwt.t -> mechanism_result Lwt.t =
	fun thread ->
		try_lwt
			thread >> return Complete
		with e -> return (Error e)

let run_mechanisms : ('a -> unit Lwt.t) list -> 'a -> mechanism_result Lwt.t =
	fun mechanisms arg -> Lwt.pick (
		mechanisms |> List.map (fun mech ->
			try_lwt
				log#debug "mech start...";
				lwt () = mech arg in
				log#debug "mech complete; pausing...";
				lwt () = pause () in
				log#debug "WHAT ? pause ended!";
				pause ()
			with e -> return (Error e)
		)
	)



class type fragment_t = object
	method attach : #Dom.node Js.t -> unit Lwt.t
end

class type ['a] widget_t = object
	inherit fragment_t
	method attach_widget : #Dom.node Js.t -> ('a Js.t * unit Lwt.t)
	method attach_widget_pure : #Dom.node Js.t -> ('a Js.t * mechanism_result Lwt.t)
end

class virtual ['a] widget_base mechanisms (children:#fragment_t list ref) =
	let mechanisms = ref mechanisms in
object (self)
	method virtual create_elem : 'a Js.t
	method attach_widget : 'p. (#Dom.node as 'p) Js.t -> ('a Js.t * unit Lwt.t) = fun parent ->
		let elem: 'a Js.t = self#create_elem in
		Dom.appendChild parent elem;
		let attach_child : #fragment_t -> unit Lwt.t = (fun frag -> frag#attach elem) in
		let child_threads = !children |> List.map attach_child in
		(* Console.console##log_3( *)
		(* 	Js.string"Element: ", *)
		(* 	elem, *)
		(* 	Js.string(" running " ^ (string_of_int (List.length !mechanisms)) ^ " mechanisms") *)
		(* ); *)
		let mechanism_threads = !mechanisms |> List.map (fun mech -> mech elem) in
		(
			elem,
			Lwt.join @@ List.concat [
				child_threads;
				mechanism_threads
			]
		)

	method attach_widget_pure : 'p. (#Dom.node as 'p) Js.t -> ('a Js.t * mechanism_result Lwt.t) = fun parent ->
		let elem: 'a Js.t = self#create_elem in
		Dom.appendChild parent elem;
		let attach_child : #fragment_t -> unit Lwt.t = fun frag -> frag#attach elem in
		let run_mechanism mech = mech elem in
		(
			elem,
			pick_failed (List.concat [
				!mechanisms |> List.map run_mechanism |> List.map convert_exception;
				!children   |> List.map attach_child  |> List.map convert_exception
			])
		)

	method attach: 'p. (#Dom.node as 'p) Js.t -> unit Lwt.t = fun parent ->
		snd @@ self#attach_widget parent

	method mechanism mech = mechanisms := mech::!mechanisms
end

(* an Element based widget - can have attrs and children *)
class ['a] widget ?(mechanisms=[]) (cons:unit -> #Dom_html.element Js.t) (children:#fragment_t list) =
	let children = ref children in
	let attrs = ref StringMap.empty in
object (self)
	inherit ['a] widget_base mechanisms children
	method create_elem =
		let elem = cons () in
		!attrs |> StringMap.iter (fun key value ->
			elem##setAttribute((Js.string key), (Js.string value))
		);
		elem

	method attr (name:string) (value:string) = attrs := StringMap.add name value !attrs
	method append : 'c. (#fragment_t as 'c) -> unit = fun child -> children := List.append !children [(child:>fragment_t)]
	method append_all (new_children:#fragment_t list) = children := List.append !children new_children
	method prepend : 'c. (#fragment_t as 'c) -> unit = fun child -> children := (child:>fragment_t)::!children
end

(* a plain DOM node widget that can't have attributes or children
 * (e.g Text / Comment node *)
class ['a] leaf_widget ?(mechanisms=[]) (cons:unit -> #Dom.node Js.t) =
object (self)
	inherit ['a] widget_base mechanisms (ref [])
	method create_elem = cons ()
end


let non_null o = Js.Opt.get o (fun () -> raise (AssertionError "unexpected null"))

(* let stream_mechanism s = fun elem -> *)
(* 	let current:(Dom.node Js.t ref) = ref elem in *)
(* 	let parent = ref None in *)
(* 	s |> Lwt_stream.iter (fun new_val -> *)
(* 		let p = (match !parent with *)
(* 			| Some p -> p *)
(* 			| None -> *)
(* 					let p = (!current##parentNode) |> non_null in *)
(* 					parent := Some p; *)
(* 					p *)
(* 		) in *)
(* 		Dom.replaceChild p new_val !current; *)
(* 		current := new_val *)
(* 	) *)
let stream_mechanism s = fun elem ->
	let current:(Dom.node Js.t ref) = ref elem in
	let parent = ref None in
	let effect = s |> S.map (fun new_val ->
		let p = (match !parent with
			| Some p -> p
			| None ->
					let p = (!current##parentNode) |> non_null in
					parent := Some p;
					p
		) in
		Dom.replaceChild p new_val !current;
		current := new_val
	) in
	try_lwt
		pause ()
	finally
		S.stop ~strong:true effect;
		Lwt.return_unit

let node_signal_of_string str_sig = str_sig |> S.map (fun str ->
	((Dom_html.document##createTextNode(Js.string str)):>Dom.node Js.t)
)

let create_blank_node () =
	let elem = Dom_html.document##createComment(Js.string "placeholder") in
	(elem:>Dom.node Js.t)

let stream s = new leaf_widget ~mechanisms:[stream_mechanism s] create_blank_node
let text_stream s = new leaf_widget ~mechanisms:[stream_mechanism (node_signal_of_string s)] create_blank_node

let wrap : (Dom_html.document Js.t -> 'a Js.t) -> ?children:(fragment_t list as 'c) -> Dom_html.document Js.t -> 'a widget =
	fun cons ?(children=[]) doc ->
		new widget (fun () -> cons doc) (children:>(fragment_t list))

let element cons = new widget cons []
let textArea = wrap Dom_html.createTextarea
let div = wrap Dom_html.createDiv
let form = wrap Dom_html.createForm
let label = wrap Dom_html.createLabel
let a = wrap Dom_html.createA
let input = wrap Dom_html.createInput
let text t doc : Dom.text leaf_widget = new leaf_widget (fun () -> doc##createTextNode (Js.string t))

let stop event =
	Dom.preventDefault event;
	Dom_html.stopPropagation event

let withContent : #Dom.node Js.t -> 'w #widget_t -> ('w Js.t -> unit Lwt.t) -> unit Lwt.t =
	fun parent content block ->
		let ((elem:'w Js.t), block_mech) = content#attach_widget_pure parent in
		try_lwt
			Lwt.pick [
				(
					lwt result = block_mech in
					match result with
						| Complete -> assert false
						| Error e -> Lwt.fail e
				);
				block elem
			]
		finally (
			Dom.removeChild parent (elem:>Dom.node Js.t);
			Lwt.return_unit
		)

 
