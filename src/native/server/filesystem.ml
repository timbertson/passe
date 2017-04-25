open Lwt
open Result

module Log = (val Passe.Logging.log_module "fs")
module type FS = sig
	include Mirage_fs_lwt.S
		with type +'a io = 'a Lwt.t
		(* with type error := Mirage_fs.error *)
		(* and type write_error := Mirage_fs.write_error *)
	(* with type page_aligned_buffer = Cstruct.t *)
end

(* type error = Mirage_fs.error *)
(* type write_error = Mirage_fs.write_error *)
type stat = Mirage_fs.stat

module type FSCommonSig = sig
	include FS
	type write_instruction = [ `Output of string | `Rollback ]
	type write_commit = [ `Commit | `Rollback ]
	type exception_reason =
		| ENOENT
		| FS_ERROR of string

	exception FsError of exception_reason

	val fail : string -> error -> 'a
	val fail_write : string -> write_error -> 'a

	val unwrap : string -> ('a, error) result -> 'a
	val unwrap_write : string -> ('a, write_error) result -> 'a

	val unwrap_lwt : string -> ('a, error) result Lwt.t -> 'a Lwt.t
	val unwrap_write_lwt : string -> ('a, write_error) result Lwt.t -> 'a Lwt.t

	(* shouldn't this be in the mirage FS signature? *)
	(* val error_message : error -> string *)

	val destroy_if_exists : t -> string -> (unit, write_error) result Lwt.t

	val ensure_empty : t -> string -> unit Lwt.t

	val ensure_exists : t -> string -> unit Lwt.t

	val stat : t -> string -> (stat, error) result Lwt.t

	val mkdir : t -> string -> (unit, write_error) result Lwt.t

	(* TODO foo_exn versions of all the above functions for convenience? *)

end

module type Sig = sig
	(* TODO expose only a subset *)
	include FSCommonSig

	val write_file_s : t -> string -> ?proof:Lock.proof -> write_instruction Lwt_stream.t -> unit Lwt.t
	val write_file : t -> string -> ?proof:Lock.proof -> string -> unit Lwt.t
	val read_file_s : t -> string -> ?proof:Lock.proof -> (Lock.proof -> string Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t
	val read_file : t -> ?proof:Lock.proof -> string -> string Lwt.t
end

module type AtomicSig = functor(Fs:FSCommonSig) -> sig
	type write_commit = Fs.write_commit
	val with_writable: Fs.t -> string -> (string -> write_commit Lwt.t) -> unit Lwt.t
	val readable:  Fs.t -> string -> string Lwt.t
end

module MakeCommon (Fs:FS) : (FSCommonSig with type t = Fs.t) = struct
	include Fs

	type write_instruction = [ `Output of string | `Rollback ]
	type write_commit = [ `Commit | `Rollback ]
	type exception_reason =
		| ENOENT
		| FS_ERROR of string

	exception FsError of exception_reason

	let _error_message pp e = Format.asprintf "%a" pp e

	let () = Printexc.register_printer (function
		| FsError ENOENT -> Some "ENOENT"
		| FsError (FS_ERROR e) -> Some ("FS_ERROR: " ^ e)
		| _ -> None
	)

	let _fail pp desc e =
		let errmsg = _error_message pp e in
		Log.debug (fun m->m "%s operation failed: %s" desc errmsg);
		raise (FsError (match e with
			| `No_directory_entry -> ENOENT
			| _ -> FS_ERROR errmsg
		))

	let fail = _fail pp_error
	let fail_write = _fail pp_write_error

	let unwrap desc = function
		| Ok x -> x
		| Error e -> fail desc e

	let unwrap_write desc = function
		| Ok x -> x
		| Error e -> fail_write desc e

	let unwrap_lwt desc = Lwt.map (unwrap desc)
	let unwrap_write_lwt desc = Lwt.map (unwrap_write desc)

	(* returns whether the file was created *)
	let _ensure_exists fs path =
		match_lwt stat fs path with
			| Ok _ -> return_false
			| Error `No_directory_entry ->
					lwt () = create fs path |> unwrap_write_lwt "create" in
					return_true
			| Error err -> fail "stat" err

	let ensure_exists fs path =
		lwt (_created:bool) = _ensure_exists fs path in
		return_unit

	let ensure_empty fs path =
		(* Note: not atomic *)
		lwt created = _ensure_exists fs path in
		if not created then begin
			(* recreate to truncate *)
			lwt () = destroy fs path |> unwrap_write_lwt "destroy" in
			create fs path |> unwrap_write_lwt "create"
		end else return_unit
	
	let destroy_if_exists fs path =
		match_lwt destroy fs path with
			| Ok _ | Error `No_directory_entry -> return (Ok ())
			| Error _ as e -> return e
end

module StringMap = Map.Make(String)
module MutexMap = StringMap
module Make (Fs:FS)(Atomic:AtomicSig) : (Sig with type t = Fs.t) = struct
	module Common = MakeCommon(Fs)
	module Atomic = Atomic(Common)
	include Common
	let locks = ref StringMap.empty
	let _with_lock : 'a. string -> ?proof:Lock.proof -> (Lock.proof -> 'a Lwt.t) -> 'a Lwt.t = fun path ?proof fn ->
		let lock =
			try StringMap.find path !locks
			with Not_found -> begin
				let lock = Lock.create () in
				locks := StringMap.add path lock !locks;
				lock
			end in
		try_lwt
			Lock.use ?proof lock fn
		finally (
			Log.debug (fun m->m "finished with mutex %s; no_open_locks = %b" path (Lock.is_empty lock));
			if (Lock.is_empty lock) then (
				locks := StringMap.remove path !locks
			);
			return_unit
		)

	let locked_atomic_write_exn fs path ?proof fn = _with_lock ?proof path (fun proof ->
		Atomic.with_writable fs path (fn proof)
	)

	let locked_atomic_read_exn fs path ?proof fn = _with_lock ?proof path (fun proof ->
		lwt read_path = Atomic.readable fs path in
		fn proof read_path
	)

	let write_file_s fs path ?proof stream =
		locked_atomic_write_exn fs path ?proof (fun _proof path ->
			Log.debug (fun m->m "Writing file stream: %s" path);
			let offset = ref 0 in
			let result = ref `Commit in
			lwt () = stream |> Lwt_stream.iter_s (fun instruction ->
				match instruction with
					| `Rollback ->
						result := `Rollback;
						Log.debug (fun m->m "aborted write at offset %d" !offset);
						return_unit
					| `Output chunk ->
						let size = String.length chunk in
						let prev_offset = !offset in
						offset := !offset + size;
						Log.debug (fun m->m "writing chunk of len %d to %s at offset %d" size path prev_offset);
						unwrap_write_lwt "write" (write fs path prev_offset (Cstruct.of_string chunk))
			) in
			let () = match !result with
				| `Commit -> Log.debug (fun m->m "comitting file write: %s" path)
				| _ -> () in
			return !result
		)

	let write_file fs path ?proof contents =
		write_file_s fs path ?proof (Lwt_stream.of_list [`Output contents])

	let read_file_s = fun fs path ?proof consumer ->
		locked_atomic_read_exn fs ?proof path (fun proof path ->
			Log.debug (fun m->m "Reading file stream: %s" path);
			let offset = ref 0 in
			let max_chunk_size = 4096 in
			consumer proof (Lwt_stream.from (fun () ->
				let rv = ref "" in
				let chunk_size = ref 0 in
				lwt chunks = Common.read fs path !offset max_chunk_size in
				let () = match chunks with
				(* TODO: mutate `rv` rather than allocating each chunk *)
					| Ok chunks ->
						chunks |> List.iter (fun chunk ->
							let chunk = Cstruct.to_string chunk in
							chunk_size := !chunk_size + String.length chunk;
							rv := (!rv ^ chunk)
						);
						Log.debug (fun m->m "read %d chunks from %s[%d] => %db" (List.length chunks) path !offset !chunk_size);
						offset := !offset + !chunk_size
					| Error e -> fail "read" e
				in
				return (
					if !chunk_size = 0 then None else (Some !rv)
				)
			))
		)

	let read_file fs ?proof path =
		read_file_s fs path ?proof (fun _proof stream -> Lwt_stream.fold (^) stream "")

end
