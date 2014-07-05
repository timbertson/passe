let opt_map = Option.map
include React
let editable_signal source =
	let derived, update = S.create (S.value source) in
	let effect = source |> S.map update in
	ignore @@ S.retain derived (fun () -> ignore effect; ());
	(derived, update)

let signal_lift_opt (fn:'a -> 'b) (opt: 'a option signal) : 'b option signal =
	S.map (fun v -> opt_map fn v) opt
