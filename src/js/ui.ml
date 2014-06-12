open Common
open Lwt
let log = Logging.get_logger "ui"

(* FIXME: only required because raised exceptions don't seem to propagate properly
 * between #attach and withContent *)
type mechanism_result =
	| Complete
	| Error of exn

let run_mechanisms : ('a -> unit Lwt.t) list -> 'a -> mechanism_result Lwt.t =
	fun mechanisms arg -> Lwt.pick (
		mechanisms |> List.map (fun mech ->
			try_lwt
				mech arg >> return Complete
			with e -> return (Error e)
		)
	)

let convert_exception : unit Lwt.t -> mechanism_result Lwt.t =
	fun thread ->
		try_lwt
			thread >> return Complete
		with e -> return (Error e)


class type fragment = object
	method attach : #Dom.node Js.t -> unit Lwt.t
end

class type ['a] widget = object
	inherit fragment
	method attach_widget : #Dom.node Js.t -> ('a Js.t * unit Lwt.t)
	method attach_widget_pure : #Dom.node Js.t -> ('a Js.t * mechanism_result Lwt.t)
end

class ['a] element ?(mechanisms=[]) (cons:unit -> #Dom.node Js.t) (children:#fragment list) =
object (self)
	method attach_widget : 'p. (#Dom.node as 'p) Js.t -> ('a Js.t * unit Lwt.t) = fun parent ->
		let elem: 'a Js.t = cons () in
		Dom.appendChild parent elem;
		let attach_child : #fragment -> unit Lwt.t = fun frag -> frag#attach elem in
		let child_threads = children |> List.map attach_child in
		let mechanism_threads = mechanisms |> List.map (fun mech -> mech elem) in
		(
			elem,
			try_lwt
				Lwt.join @@ List.concat [
					child_threads;
					mechanism_threads
				]
			finally (
				Console.console##log_2(Js.string"attach_widget done, removing", elem);
				Dom.removeChild parent elem;
				Lwt.return_unit
			)
		)

	method attach_widget_pure : 'p. (#Dom.node as 'p) Js.t -> ('a Js.t * mechanism_result Lwt.t) = fun parent ->
		let elem: 'a Js.t = cons () in
		Dom.appendChild parent elem;
		let attach_child : #fragment -> unit Lwt.t = fun frag -> frag#attach elem in
		let child_threads = children |> List.map attach_child in
		let child_results = List.map convert_exception child_threads in
		(
			elem,
			Lwt.pick (run_mechanisms mechanisms elem :: child_results)
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
		let p = (match !parent with
			| Some p -> p
			| None ->
					let p = (!current##parentNode) |> non_null in
					parent := Some p;
					p
		) in
		Dom.replaceChild p new_val !current;
		current := new_val
	)

let create_blank_node () =
	let elem = Dom_html.document##createComment(Js.string "placeholder") in
	(elem:>Dom.node Js.t)

let stream s = new element ~mechanisms:[stream_mechanism s] create_blank_node []

let textArea doc = new element (fun () -> Dom_html.createTextarea doc) []
let div ~children doc = new element (fun () -> Dom_html.createDiv doc) children

let pause () = Lwt.wait () |> fst

let withContent : #Dom.node Js.t -> 'w widget -> ('w Js.t -> unit Lwt.t) -> unit Lwt.t =
	fun parent content block ->
		let ((elem:'w Js.t), block_mech) = content#attach_widget_pure parent in
		try_lwt
			Lwt.pick [
				(
					try_lwt
						log#info "MECH START";
						lwt result = block_mech in
						match result with
							| Complete -> pause ()
							| Error e -> Lwt.fail e
					finally (
						log#info "MECH ENDED";
						Lwt.return_unit
					)
				);
				block elem
			]
		finally (
			Dom.removeChild parent (elem:>Dom.node Js.t);
			Lwt.return_unit
		)

 
