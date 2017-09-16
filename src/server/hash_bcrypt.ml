module Impl = struct
	type t = Bcrypt.hash
	let serialize = Bcrypt.string_of_hash
	let hash ~(count:int) ~(seed:string) s = Bcrypt.hash ~count ~seed s
	let alg = "bcrypt"
end
let () = Hash._register (module Impl)
include Impl
