module Impl = struct
	type t = Bcrypt.hash_t
	let to_string = Bcrypt.string_of_hash
	let of_string = Bcrypt.hash_of_string
	let hash ~(count:int) ~(seed:string) s =
		Bcrypt.hash ~count ~seed s
	let verify = Bcrypt.verify
	let alg = "bcrypt"
end
let () = Hash._register (module Impl)
include Impl
