open Passe
open Common
open Lwt
open Rresult
open Astring
module Path = FilePath.UnixPath.Abstract

module Log = (val Logging.log_module "static")

type error = [ `Not_found | `Invalid_path | `Read_error of string ]

let string_of_error pp e = Format.asprintf "%a" pp e

module type Sig = sig
	type t
	val read_s : t -> string -> (string Lwt_stream.t -> 'a Lwt.t) -> ('a, error) result Lwt.t
	val etag : t -> string -> (string Lwt_stream.t -> string Lwt.t) -> (string, error) result Lwt.t
end

module type Kv_RO = sig
	include Mirage_kv.RO
	with type page_aligned_buffer = Cstruct.t
	and type 'a io = 'a Lwt.t
end

module Kv(Impl:Kv_RO) : sig
	include Sig
	val init : Impl.t -> t
end = struct
	let map_error : Impl.error -> error = function
		| `Unknown_key _ -> `Not_found
		| e -> `Read_error (string_of_error Impl.pp_error e)

	let reword_error res = R.reword_error map_error res

	type t = Impl.t * string StringMap.t ref
	let init t = (t, ref StringMap.empty)

	let read_s (t,_) path fn =
		Impl.size t path |> Lwt_r.bind (fun (size:int64) ->
			Impl.read t path 0L size |> Lwt_r.bind_lwt (fun contents ->
				fn (Lwt_stream.of_list contents |> Lwt_stream.map Cstruct.to_string)
			)
		) |> Lwt.map reword_error

	let etag (t, cache) path fn : (string, error) result Lwt.t =
		match (try Some (StringMap.find path !cache) with Not_found -> None) with
			| Some cached -> return (Ok cached)
			| None ->
				read_s (t, cache) path fn |> Lwt_r.map (fun (etag:string) ->
					cache := StringMap.add path etag !cache;
					etag
				)
end

module Fs(Impl:Filesystem.Sig) : sig
	include Sig
	val init : fs:Impl.t -> string -> t
end = struct
	type t = Impl.t * string
	let init ~fs root : t = (fs, root)

	let path_of_string (_,root) path =
		Log.debug (fun m->m "Normalizing path %s against %s" path root);
		let root = Path.make_filename [root] in
		assert (not (Path.is_relative root));

		let path = Path.make_filename [path] in
		assert (Path.is_relative path);

		Path.concat root path

	let _error_message pp e = Format.asprintf "%a" pp e

	let read_s t path fn : ('a, error) result Lwt.t =
		let (fs, _root) = t in
		let path = path_of_string t path in
		(* TODO: use result in FS instead of try/catch here *)
		try_lwt
			(Impl.read_file_s fs (Path.string_of_filename path) (fun _proof -> fn)) |> Lwt.map R.ok
		with Impl.FsError e -> return (match e with
			| Impl.ENOENT -> Error `Not_found
			| Impl.FS_ERROR e -> Error (`Read_error e)
		)

	let etag = read_s
end

let key_of_path_components parts : (string, error) result =
	let invalid_component = function
		| "" -> true
		| part -> String.is_prefix ~affix:"." part || String.is_infix ~affix:"/" part
	in
	if parts |> List.any invalid_component
		then Error `Invalid_path
		else Ok (String.concat ~sep:"/" parts)

