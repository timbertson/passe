open Common
open Lwt
open Lwt_react
let log = Logging.get_logger "ui"

(* FIXME: only required because raised exceptions don't seem to propagate properly
 * between #attach and withContent *)
type mechanism_result =
	| Complete
	| Error of exn

let pause _ = Lwt.wait () |> fst

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

let effectful_stream_mechanism effect : unit Lwt.t =
	try_lwt
		log#info "starting effectful mechanism";
		pause ()
	finally
		log#info "stopping effectful mechanism";
		S.stop ~strong:true effect;
		Lwt.return_unit
	

let stream_attribute_mechanism name value = fun elem ->
	let set v = elem##setAttribute(name, Js.string v) in
	effectful_stream_mechanism (value |> S.map set)

let stream_class_mechanism name value = fun elem ->
	let name_js = Js.string name in
	let set v =
		log#info "setting class %s to %b" name v;
		let cls = elem##classList in
		if v then cls##add(name_js) else cls##remove(name_js)
	in
	effectful_stream_mechanism (value |> S.map set)


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
	method attr_s (name:string) (value:string signal) = self#mechanism (stream_attribute_mechanism name value)
	method class_s (name:string) (value:bool signal) = self#mechanism (stream_class_mechanism name value)
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

let create_blank_node _ =
	let elem = Dom_html.document##createComment(Js.string "placeholder") in
	(elem:>Dom.node Js.t)

let wrap_leaf : (Dom_html.document Js.t -> 'a Js.t)
	-> 'a leaf_widget
	=
	fun cons -> new leaf_widget (fun () -> cons Dom_html.document)

let text t : Dom.text leaf_widget = new leaf_widget (fun () -> Dom_html.document##createTextNode (Js.string t))
let none doc : Dom.node leaf_widget = new leaf_widget create_blank_node

type ('a,'b) listy = (('a * 'b) list)
type 'a children = #fragment_t list as 'a

type 'w widget_constructor = (?children:(fragment_t list)
	-> ?text:(string)
	-> ?cls:(string)
	-> ?attrs:((string * string) list)
	-> unit -> 'w widget)

let wrap : (Dom_html.document Js.t -> 'a Js.t) -> 'a widget_constructor =
	fun cons ?(children=[]) ?text:t ?cls ?attrs () ->
		let rv = new widget (fun () -> cons Dom_html.document) (children:>(fragment_t list)) in
		t |> Option.may (fun t -> rv#append (text t));
		cls |> Option.may (fun t -> rv#attr "class" t);
		attrs |> Option.may (fun attrs -> attrs |> List.iter (fun (k,v) -> rv#attr k v));
		rv

let element cons = new widget cons []
let textArea: Dom_html.textAreaElement widget_constructor = wrap Dom_html.createTextarea
let div = wrap Dom_html.createDiv
let span = wrap Dom_html.createSpan
let form = wrap Dom_html.createForm
let label = wrap Dom_html.createLabel
let a = wrap Dom_html.createA
let input = wrap Dom_html.createInput

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

let stream_mechanism s = fun (elem:#Dom.node Js.t) ->
	let new_widget = Lwt_condition.create () in
	let effect : unit S.t = s |> S.map (Lwt_condition.signal new_widget) in
	try_lwt
		let p = (elem##parentNode) |> non_null in
		let widget = ref @@ S.value s in
		while_lwt true do
			withContent p !widget (fun _ ->
				(* wait until next update *)
				lwt w = Lwt_condition.wait new_widget in
				widget := w;
				Lwt.return_unit
			)
		done
	finally
		S.stop ~strong:true effect;
		Lwt.return_unit

let node_signal_of_string str_sig = str_sig |> S.map text

let stream s = new leaf_widget ~mechanisms:[stream_mechanism s] create_blank_node
let text_stream s = new leaf_widget ~mechanisms:[stream_mechanism (node_signal_of_string s)] create_blank_node

let input_signal ?(events=Lwt_js_events.inputs) widget =
	let signal, update = S.create "" in
	let update elem =
		log#info "Updating text value: %s" (elem##value |> Js.to_string);
		update (elem##value |> Js.to_string) in
	widget#mechanism (fun elem ->
		update elem;
		events elem (fun event _ ->
			update elem;
			Lwt.return_unit
		)
	);
	signal

