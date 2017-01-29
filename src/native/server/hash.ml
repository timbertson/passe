open Passe.Common
module type Sig = sig
	type t
	val serialize : t -> string
	val hash : count:int -> seed:string -> string -> t
	val alg : string
end

let _impls = ref StringMap.empty
let _register (module Impl: Sig) =
	let () = _impls := StringMap.add Impl.alg (module Impl : Sig) !_impls in
	()

module Hash_sha256 = struct
	type t = Sha256.t
	let serialize : t -> string = Sha256.to_hex
	let hash ~count:(_count:int) ~(seed:string) s : t =
		prerr_endline "WARN: sha256 implementation is not cryptographically strong; use for development only";
		Sha256.string (seed ^ s)
	let alg = "sha256"
end
let () = _register (module Hash_sha256)

let select alg : (module Sig) =
	try StringMap.find alg !_impls
	with Not_found -> failwith (Printf.sprintf "unsupported hash alg: %s" alg)
