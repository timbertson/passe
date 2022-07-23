open Passe
open Lwt
open Rresult
open Common
open Astring

module Log = (val Logging.log_module "static")

module type Sig = sig
	type t
	type key
	val key : string list -> (key, Error.t) result
	val read_s : t -> key
		-> ((string, Error.t) result Lwt_stream.t -> ('a, Error.t) result Lwt.t)
		-> ('a option, Error.t) result Lwt.t

	val etag : t -> key
		-> ((string, Error.t) result Lwt_stream.t -> (string, Error.t) result Lwt.t)
		-> (string option, Error.t) result Lwt.t
end

module type Mirage_kv_RO = sig
	include Mirage_kv.RO
	with type error = Mirage_kv.error
end

module Readonly(Impl:Mirage_kv_RO) : sig
	include Sig
	val init : Impl.t -> t
end = struct
	type key = Mirage_kv.Key.t
	let key parts = Ok (Mirage_kv.Key.v (String.concat ~sep:"/" parts))

	type t = Impl.t * string option StringMap.t ref
	let init t = (t, ref StringMap.empty)
	let string_of_impl_error err = pp_strf Impl.pp_error err

	let read_s (t,_) path fn =
		let contents = Impl.get t path in
		Lwt.bind contents (function
			| Ok str -> fn (Lwt_stream.return (Ok str)) |> Lwt_r.map Option.some
			| Error (`Not_found _) -> return (Ok None)
			| Error err -> return (Error (`Failed (string_of_impl_error err)))
		)

	let etag (t, cache) path fn =
		let path_str = Mirage_kv.Key.to_string path in
		match (try Some (StringMap.find path_str !cache) with Not_found -> None) with
			| Some cached -> return (Ok cached)
			| None -> (
				read_s (t, cache) path fn |> Lwt_r.map (fun (etag:string option) ->
					cache := StringMap.add path_str etag !cache;
					etag
				)
			)
end

module Of_dynamic(Impl:Dynamic_store.Sig) : sig
	include Sig
	val init : Impl.t -> t
end = struct
	type t = Impl.t
	type key = Path.relative

	let init store = store
	let key parts = Path.make parts

	let read_s store path fn : ('a option, Error.t) result Lwt.t =
		Impl.read_s store path fn

	let etag = read_s
end
