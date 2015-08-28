open Passe.Common
module type Sig = sig
	type t
	val to_string : t -> string
	val of_string : string -> t
	val hash : count:int -> seed:string -> string -> t
	val verify : string -> t -> bool
	val alg : string
end

let _impls = ref StringMap.empty
let _register (module Impl: Sig) =
	let () = _impls := StringMap.add Impl.alg (module Impl : Sig) !_impls in
	()

module Hash_sha256 = struct
	type t = Sha256.t
	let to_string = Sha256.to_bin
	let of_string = Sha256.string
	let hash ~(count:int) ~(seed:string) s = failwith "TODO: NOT IMPLEMENTED"
	let verify str hash = (to_string (of_string str)) = (to_string hash)
	let alg = "sha256"
end
let () = _register (module Hash_sha256)

let select alg : (module Sig) =
	try StringMap.find alg !_impls
	with Not_found -> failwith (Printf.sprintf "unsupported hash alg: %s" alg)
