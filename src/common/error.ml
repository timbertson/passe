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

let raise_assert s = failwith (`AssertionError s)

let pp = fun formatter error ->
	Format.pp_print_string formatter (match error with
		| `Invalid s -> "Invalid: " ^ s
		| `AssertionError s -> "Assertion failed: " ^ s
		| `Failed s -> s
	)
