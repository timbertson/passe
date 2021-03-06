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
	with type page_aligned_buffer = Cstruct.t
	and type 'a io = 'a Lwt.t
end

module Readonly(Impl:Mirage_kv_RO) : sig
	include Sig
	val init : Impl.t -> t
end = struct
	type key = string
	let key parts = Ok (String.concat ~sep:"/" parts)

	type t = Impl.t * string option StringMap.t ref
	let init t = (t, ref StringMap.empty)
	let string_of_impl_error err = pp_strf Impl.pp_error err

	let read_s (t,_) path (fn) =
		let contents = Impl.size t path |> Lwt_r.bind (fun (size:int64) ->
			Impl.read t path 0L size |> Lwt_r.map (fun contents ->
				Lwt_stream.of_list contents |> Lwt_stream.map (R.ok % Cstruct.to_string)
			)
		) in
		Lwt.bind contents (function
			| Ok stream -> fn stream |> Lwt_r.map Option.some
			| Error (`Unknown_key _) -> return (Ok None)
			| Error err -> return (Error (`Failed (string_of_impl_error err)))
		)

	let etag (t, cache) path fn =
		match (try Some (StringMap.find path !cache) with Not_found -> None) with
			| Some cached -> return (Ok cached)
			| None ->
				read_s (t, cache) path fn |> Lwt_r.map (fun (etag:string option) ->
					cache := StringMap.add path etag !cache;
					etag
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
