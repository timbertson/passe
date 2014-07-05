open Common
open Lwt
open React
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
	method attach_widget_pure :
		?before:(Dom.node Js.t) ->
		#Dom.node Js.t -> ('a Js.t * mechanism_result Lwt.t)
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

	method attach_widget_pure : 'p.
		?before:(Dom.node Js.t) ->
		(#Dom.node as 'p) Js.t ->
		('a Js.t * mechanism_result Lwt.t)
		=
		fun ?before parent ->
		let elem: 'a Js.t = self#create_elem in
		let before = Js.Opt.option before in
		Dom.insertBefore parent elem before;
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

let create_text_node t _ =
	let elem = Dom_html.document##createTextNode (Js.string t) in
	(elem:>Dom.node Js.t)

let text t : Dom.node leaf_widget = new leaf_widget (create_text_node t)
let empty () : Dom.node leaf_widget = new leaf_widget create_blank_node

type ('a,'b) listy = (('a * 'b) list)
type 'a children = #fragment_t list as 'a

type ('m, 'w) widget_constructor = (
	?children:(fragment_t list)
	-> ?mechanism:('m Js.t -> unit Lwt.t)
	-> ?text:(string)
	-> ?cls:(string)
	-> ?attrs:((string * string) list)
	-> unit -> 'w)

let frag f = (f:>fragment_t)
let space = frag (text " ")

let wrap : (Dom_html.document Js.t -> 'a Js.t) -> ('a, 'a widget) widget_constructor =
	fun cons ?(children=[]) ?mechanism ?text:t ?cls ?attrs () ->
		let rv = new widget (fun () -> cons Dom_html.document) (children:>(fragment_t list)) in
		t |> Option.may (fun t -> rv#append (text t));
		cls |> Option.may (fun t -> rv#attr "class" t);
		mechanism |> Option.may (fun m -> rv#mechanism m);
		attrs |> Option.may (fun attrs -> attrs |> List.iter (fun (k,v) -> rv#attr k v));
		rv

let child f : ('a, fragment_t) widget_constructor =
	fun ?children ?mechanism ?text ?cls ?attrs () ->
	((f ?children ?mechanism ?text ?cls ?attrs ()):>fragment_t)

let element cons = new widget cons []
let textArea = wrap Dom_html.createTextarea
let div = wrap Dom_html.createDiv
let span = wrap Dom_html.createSpan
let form = wrap Dom_html.createForm
let label = wrap Dom_html.createLabel
let a = wrap Dom_html.createA
let p = wrap Dom_html.createP
let strong = wrap Dom_html.createStrong
let h1 = wrap Dom_html.createH1
let h2 = wrap Dom_html.createH2
let h3 = wrap Dom_html.createH3
let ul = wrap Dom_html.createUl
let li = wrap Dom_html.createLi
let input = wrap Dom_html.createInput
let i = wrap Dom_html.createI

let stop event =
	Dom.preventDefault event;
	Dom_html.stopPropagation event

let withContent :
	#Dom.node Js.t ->
	?before:(#Dom.node Js.t) ->
	'w #widget_t -> ('w Js.t -> unit Lwt.t) -> unit Lwt.t
	=
	fun parent ?before content block ->
		let ((elem:'w Js.t), block_mech) = content#attach_widget_pure ?before parent in
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

let stream_mechanism (s:#Dom.node #widget_t S.t) = fun (placeholder:#Dom.node Js.t) ->
	let new_widget = Lwt_condition.create () in
	let effect : unit S.t = s |> S.map (fun w ->
		Lwt_condition.signal new_widget (w:>Dom.node widget_t)) in
	let p = (placeholder##parentNode) |> non_null in
	let elem = ref placeholder in
	try_lwt
		let widget = ref @@ ((S.value s):>Dom.node widget_t) in
		while_lwt true do
			withContent p ~before:placeholder !widget (fun new_elem ->
				elem := new_elem;
				(* wait until next update *)
				lwt w = Lwt_condition.wait new_widget in
				widget := w;
				Lwt.return_unit
			)
		done
	finally
		S.stop ~strong:true effect;
		Lwt.return_unit


let stream s =
	let w = new leaf_widget
		~mechanisms:[stream_mechanism s]
		create_blank_node in
	(w:>fragment_t)

let option_stream s : fragment_t =
	let empty = empty () in
	stream (s |> S.map (fun value -> match value with
		| Some value -> (value:>Dom.node widget_t)
		| None -> (empty:>Dom.node widget_t)
	))

let node_signal_of_string str_sig : (Dom.node leaf_widget) signal = str_sig |> S.map text
let text_stream s : fragment_t = stream (node_signal_of_string s)

type 'a editable = (<value:Js.js_string Js.t Js.prop; ..> as 'a) Js.t

let editable_of_signal ?(events=Lwt_js_events.inputs) ~(cons:(unit -> 't editable) ) ?update source =
	let clear_error elem = elem##classList##remove(Js.string"error") in
	let set_error elem = elem##classList##add(Js.string"error") in

	let widget:<value:Js.js_string Js.t Js.prop; ..> widget = element cons in

	let update_loop elem = match update with
		| None -> Lwt.return_unit
		| Some update -> events elem (fun event _ ->
			log#info "responding to input change (%s)" (elem##value |> Js.to_string);
			clear_error elem;
			begin
				try update (elem##value |> Js.to_string)
				with err -> set_error elem
			end;
			Lwt.return_unit
		)
	in

	let watch_loop elem =
		effectful_stream_mechanism (source |> S.map (fun v ->
			log#info "responding to signal change (%s)" v;
			clear_error elem;
			elem##value <- (Js.string v)
		))
	in

	widget#mechanism (fun elem -> watch_loop elem <&> update_loop elem);
	widget

let input_of_signal ?(events=Lwt_js_events.inputs) ?cons ?update source =
	let cons = match cons with Some c -> c | None -> (
		fun () -> Dom_html.createInput Dom_html.document ~_type:(Js.string"text")
	) in
	editable_of_signal ~events ~cons ?update source

let textarea_of_signal ?(events=Lwt_js_events.inputs) ?cons  ?update source =
	let cons = match cons with Some c -> c | None -> (
		fun () -> Dom_html.createTextarea Dom_html.document
	) in
	editable_of_signal ~events ~cons ?update source

let signal_of_widget
	~events
	~(initial:'t)
	~(get_value: ('e Js.t -> 't))
	(widget:'e widget): 't S.t
=
	let signal, update = S.create initial in
	let update elem = update (get_value elem) in
	widget#mechanism (fun elem ->
		update elem;
		events elem (fun event _ ->
			update elem;
			Lwt.return_unit
		)
	);
	signal

let signal_of_input ?(events=Lwt_js_events.inputs) widget = signal_of_widget
	~events
	~get_value:(fun elem -> elem##value |> Js.to_string)
	~initial:""
	widget

let signal_of_checkbox ~(initial:bool) (widget:Dom_html.inputElement widget) : bool S.t = signal_of_widget
	~events:Lwt_js_events.changes
	~initial
	~get_value:(fun elem -> elem##checked |> Js.to_bool)
	widget

let optional_signal_content : ('a -> #Dom.node #widget_t) -> 'a option React.signal -> Dom.node widget_t signal =
	fun f signal ->
		let empty = empty () in
		signal |> S.map (fun value -> match value with
			| Some value -> ((f value):>Dom.node widget_t)
			| None -> (empty:>Dom.node widget_t)
		)

