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

let string_of_hex : Hex.t -> string = function
	| `Hex s -> s

module Hash_sha256 = struct
	open Mirage_crypto.Hash
	type t = Cstruct.t
	let serialize : t -> string = fun t -> Hex.of_cstruct t |> string_of_hex
	let hash ~count:(_count:int) ~(seed:string) s : t =
		prerr_endline "WARN: sha256 implementation is not cryptographically strong; use for development only";
		SHA256.digest (seed ^ s |> Cstruct.of_string)
	let alg = "sha256"
end
let () = _register (module Hash_sha256)

let select alg : (module Sig) =
	try StringMap.find alg !_impls
	with Not_found -> failwith (Printf.sprintf "unsupported hash alg: %s" alg)
