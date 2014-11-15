type url = [
	| `Absolute of Uri.t
	| `Path of string list
	]

let path p = `Path p

let canonicalize ~root = function
	| `Absolute u -> u
	| `Path p -> Uri.with_path root ("/" ^ String.concat "/" p)
