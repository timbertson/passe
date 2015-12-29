module Impl = struct
	type t = Bcrypt.hash_t
	let to_hex = Bcrypt.string_of_hash
	let hash ~(count:int) ~(seed:string) s =
		Bcrypt.hash ~count ~seed s
	let verify ~expected s = Bcrypt.verify s (Bcrypt.hash_of_string expected)
	let alg = "bcrypt"
end
let () = Hash._register (module Impl)
include Impl
