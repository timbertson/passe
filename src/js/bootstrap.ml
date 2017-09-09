open Passe
open Common
open Vdoml
open Html
open Js_util

let icon name = let open Vdoml.Html in i ~a:[a_class ("glyphicon glyphicon-"^name)] []

type col_scale = [ `XS | `Sml | `Med | `Lg ]
let string_of_col_scale = function
	| `XS -> "xs"
	| `Sml -> "sm"
	| `Med -> "md"
	| `Lg -> "lg"

type col_spec = {
	col_scale: col_scale;
	col_size : int;
	col_offset : int option;
	col_cls : string option;
}

type col_partial = {
	cp_scale: col_scale option;
	cp_size : int option;
	cp_offset : int option;
	cp_cls : string option;
}

let class_of_col_spec s =
	let scale = string_of_col_scale s.col_scale in
	[
		Some ("col col-" ^scale^"-"^(string_of_int s.col_size));
		s.col_offset |> Option.map (fun offset -> "col-"^scale^"-offset-"^(string_of_int offset));
		s.col_cls;
	] |> List.filter_map identity |> String.concat " "

let col ?scale ?size ?offset ?cls children =
	({cp_scale = scale; cp_size = size; cp_offset = offset; cp_cls=cls}, children)

let total_row_size = 12
let row scale ?collapse ?cls children =
	let unsized_columns = children
		|> List.filter (fun (col, _) -> Option.is_none col.cp_size) in
	let remainder_size = match List.length unsized_columns with
		| 0 -> 0
		| n ->
			let taken_size = List.fold_left (fun acc (col, _) ->
				acc + (col.cp_size |> Option.default 0) + (col.cp_offset |> Option.default 0)
			) 0 children in
			(* Log.debug (fun m->m "taken size = %d, from %d cols" taken_size ((List.length children) - n)); *)
			let available_size = total_row_size - taken_size in
			(* Log.debug (fun m->m "available size = %d, split between %d" available_size n); *)
			available_size / n
	in

	let children = children |> List.map (fun (col, elem) ->
		let col = {
			col_scale = col.cp_scale |> Option.default scale;
			col_size = col.cp_size |> Option.default remainder_size;
			col_offset = col.cp_offset;
			col_cls = col.cp_cls;
		} in
		(col, elem)
	) in

	let classes = [
		if collapse = Some true then None else Some "row";
		cls
	] |> List.filter_map identity in
	div ~a:[a_class_list classes] (
		children |> List.map (fun (col, children) ->
			div ~a:[a_class (class_of_col_spec col)] children
		)
	)

let overlay instance ~cancel =
	let hooks = with_global_listeners (fun () -> [
		Js_util.global_event_listener Dom_html.Event.keydown (fun event ->
			event |> Vdoml.Event.keyboard_event |> Option.may (fun event ->
				if (event##.keyCode == Keycode.esc) then (
					Dom.preventDefault event;
					Vdoml.Ui.emit instance cancel
				)
			)
		);
	]) in
	let onclick = handler (fun evt ->
		let is_overlay = Event.target evt |> Option.map (fun target ->
			target##.classList##contains (Js.string "overlay") |> Js.to_bool
		) |> Option.default false in
		(if is_overlay
			then Some (Event.handle cancel)
			else None
		) |> Event.optional
	) in
	fun content ->
		div ~a:[a_class "overlay"; a_onclick onclick] content |> hooks

let panel ~title ?close children =
	let _title = title in
	let open Vdoml.Html in
	let header = [
		h3 ~a:[a_class "panel-title"] [text _title];
	] in
	let header = match close with
		| None -> header
		| Some close ->
			(button ~a:[
				a_onclick (emitter close);
				a_class "link pull-right close"
			] [icon "remove"]) :: header
	in
	div ~a:[a_class "panel panel-default"] [
		div ~a:[a_class "panel-heading"] header;
		div ~a:[a_class "panel-body"] children;
	]

let control_label text = label ~a:[a_class "control-label"] [Vdoml.Html.text text]
