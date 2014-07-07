let opt_map = Option.map
include React
let never_eq = (fun a b -> false)

let editable_signal source =
	let initial = (S.value source) in
	let override, update = S.create ~eq:never_eq initial in
	ignore @@ (source |> S.map (fun x -> Console.console##log(Js.string "source changed"); x));
	ignore @@ (override |> S.map (fun x -> Console.console##log(Js.string "override changed"); x));
	let combined = E.select [ S.changes override; S.changes source ] in
	let derived = S.hold initial combined in
	let derived = derived |> S.map (fun x -> Console.console##log(Js.string "editable saw change"); x)
	in
	(derived, update)

let signal_lift_opt (fn:'a -> 'b) (opt: 'a option signal) : 'b option signal =
	S.map (fun v -> opt_map fn v) opt
