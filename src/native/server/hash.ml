open Passe.Common
module type Sig = sig
	type t
	val to_hex : t -> string
	val hash : count:int -> seed:string -> string -> t
	val verify : expected:string -> string -> bool
	val alg : string
end

let _impls = ref StringMap.empty
let _register (module Impl: Sig) =
	let () = _impls := StringMap.add Impl.alg (module Impl : Sig) !_impls in
	()

module Hash_sha256 = struct
	type t = Sha256.t
	let to_hex : t -> string = Sha256.to_hex
	let hash ~(count:int) ~(seed:string) s : t =
		prerr_endline "sha256 implementation does not not support iterations; use for development only";
		Sha256.string (seed ^ s)
	let verify ~expected str = (to_hex (Sha256.string str)) = expected
	let alg = "sha256"
end
let () = _register (module Hash_sha256)

let select alg : (module Sig) =
	try StringMap.find alg !_impls
	with Not_found -> failwith (Printf.sprintf "unsupported hash alg: %s" alg)
