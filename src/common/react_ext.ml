let opt_map = Option.map
include React
let never_eq = (fun _ _ -> false)

let editable_signal source =
	let initial = (S.value source) in
	let override, update = S.create ~eq:never_eq initial in
	let combined = E.select [ S.changes override; S.changes source ] in
	let derived = S.hold initial combined in
	(derived, update)

let signal_lift_opt (fn:'a -> 'b) (opt: 'a option signal) : 'b option signal =
	S.map (fun v -> opt_map fn v) opt
