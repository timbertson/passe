let map f opt = match opt with Some x -> Some (f x) | None -> None
let default d opt = match opt with Some x -> x | None -> d
