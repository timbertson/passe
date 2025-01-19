type invalid = [
	| `Invalid of string (* will never succeed, user error *)
]

type assertion_error = [
	| `AssertionError of string (* programmer error *)
]

type failure = [
	| `Failed of string (* possibly-transitory error *)
]

type t = [ invalid | assertion_error | failure ]

exception Throwable of t

let throwable t = Throwable t

let failwith t = raise (throwable t)

let failure_of_exn e = `Failed (Printexc.to_string e)

let raise_assert s = failwith (`AssertionError s)

let to_string = function
		| `Invalid s -> "Invalid: " ^ s
		| `AssertionError s -> "Assertion failed: " ^ s
		| `Failed s -> s

let pp = fun formatter error ->
	Format.pp_print_string formatter (to_string error)

let raise_result = function
	| Ok(x) -> x
	| Error(`Msg s) -> failwith (`Failed s)
