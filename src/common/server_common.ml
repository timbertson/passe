module Make (Version:Version.Sig) = struct
	type response =
		| OK of Yojson.Safe.json
		| Unauthorized of string option
		| Failed of int * string * (Yojson.Safe.json option)


	type url = [
		| `Absolute of Uri.t
		| `Path of string list
		]

	let path p = `Path p

	let canonicalize ~root = function
		| `Absolute u -> u
		| `Path p -> Uri.with_path root ("/" ^ String.concat "/" p)

	let common_headers = [
		"x-passe-version", Version.version;
	]
end
