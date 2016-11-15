open Lwt
module Log = (val Passe.Logging.log_module "fs")
module type FS = sig
	include V1_LWT.FS with type page_aligned_buffer = Cstruct.t
end

module type FSCommonSig = sig
	include FS
	type 'a result = [ `Ok of 'a | `Error of error ]
	type write_instruction = [ `Output of string | `Rollback ]
	type write_commit = [ `Commit | `Rollback ]
	type exception_reason =
		| ENOENT of string
		| FS_ERROR of string

	exception Error of exception_reason

	val fail : string -> error -> 'a

	val unwrap : string -> 'a result -> 'a

	val unwrap_lwt : string -> 'a result Lwt.t -> 'a Lwt.t

	(* shouldn't this be in the mirage FS signature? *)
	val error_message : error -> string

	val destroy_if_exists : t -> string -> unit result Lwt.t

	val ensure_empty : t -> string -> unit Lwt.t

	val ensure_exists : t -> string -> unit Lwt.t

	val stat : t -> string -> stat result Lwt.t

	val mkdir : t -> string -> unit result Lwt.t

	(* TODO foo_exn versions of all the above functinos for convenience? *)

end

module type Sig = sig
	(* TODO expose only a subset *)
	include FSCommonSig

	val write_file_s : t -> string -> write_instruction Lwt_stream.t -> unit Lwt.t
	val write_file : t -> string -> string -> unit Lwt.t
	val read_file_s : t -> string -> (string Lwt_stream.t -> 'a Lwt.t) -> 'a Lwt.t
	val read_file : t -> string -> string Lwt.t
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
	type 'a result = [ `Ok of 'a | `Error of error ]
	type exception_reason =
		| ENOENT of string
		| FS_ERROR of string

	exception Error of exception_reason

	let error_message = function
		(* XXX this is supposed to be in the mirage FS interface? *)
		| `Block_device _ -> "Block_device error"
		| `Directory_not_empty x -> "Directory_not_empty " ^ x
		| `File_already_exists x -> "File_already_exists " ^ x
		| `Format_not_recognised x -> "Format_not_recognised " ^ x
		| `Is_a_directory x -> "Is_a_directory " ^ x
		| `No_directory_entry (parent,dir) -> "No_directory_entry " ^ parent ^ "/" ^ dir
		| `No_space -> "No_space"
		| `Not_a_directory x -> "Not_a_directory " ^ x
		| `Unknown_error x -> "Unknown_error " ^ x

	let () = Printexc.register_printer (function
		| Error (ENOENT e) -> Some ("ENOENT: " ^ e)
		| Error (FS_ERROR e) -> Some ("FS_ERROR: " ^ e)
		| _ -> None
	)

	let fail desc e =
		let errmsg = error_message e in
		Log.debug (fun m->m "%s operation failed: %s" desc errmsg);
		raise (Error (match e with
			| `No_directory_entry (parent,ent) -> ENOENT (Filename.concat parent ent)
			| _ -> FS_ERROR errmsg
		))

	let unwrap desc op =
		match op with
		| `Ok x -> x
		| `Error e -> fail desc e

	let unwrap_lwt desc = Lwt.map (unwrap desc)

	(* returns whether the file was created *)
	let _ensure_exists fs path =
		match_lwt stat fs path with
			| `Ok _ -> return_false
			| `Error (`No_directory_entry (_,_)) ->
					lwt () = create fs path |> unwrap_lwt "create" in
					return_true
			| `Error e -> fail "stat" e

	let ensure_exists fs path =
		lwt (_created:bool) = _ensure_exists fs path in
		return_unit

	let ensure_empty fs path =
		(* Note: not atomic *)
		lwt created = _ensure_exists fs path in
		if not created then begin
			(* recreate to truncate *)
			lwt () = destroy fs path |> unwrap_lwt "destroy" in
			create fs path |> unwrap_lwt "create"
		end else return_unit
	
	let destroy_if_exists fs path =
		match_lwt destroy fs path with
			| `Ok _ | `Error (`No_directory_entry (_,_)) -> return (`Ok ())
			| `Error _ as e -> return e
end

module StringMap = Map.Make(String)
module MutexMap = StringMap
module Make (Fs:FS)(Atomic:AtomicSig) : (Sig with type t = Fs.t) = struct
	module Common = MakeCommon(Fs)
	module Atomic = Atomic(Common)
	include Common
	let locks = ref StringMap.empty
	let _with_lock : 'a. string -> (unit -> 'a Lwt.t) -> 'a Lwt.t = fun path fn ->
		let lock =
			try StringMap.find path !locks
			with Not_found -> begin
				let lock = Lwt_mutex.create () in
				locks := StringMap.add path lock !locks;
				lock
			end in
		lwt () = Lwt_mutex.lock lock in
		try_lwt
			fn ()
		finally begin
			Log.debug (fun m->m "finished with mutex %s; no_open_locks = %b" path (Lwt_mutex.is_empty lock));
			if (Lwt_mutex.is_empty lock) then (
				locks := StringMap.remove path !locks
			);
			Lwt_mutex.unlock lock;
			return_unit
		end

	let locked_atomic_write_exn fs path fn = _with_lock path (fun () ->
		Atomic.with_writable fs path fn
	)

	let locked_atomic_read_exn fs path fn = _with_lock path (fun () ->
		lwt read_path = Atomic.readable fs path in
		fn read_path
	)

	let write_file_s fs path stream =
		locked_atomic_write_exn fs path (fun path ->
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
						unwrap_lwt "write" (write fs path prev_offset (Cstruct.of_string chunk))
			) in
			let () = match !result with
				| `Commit -> Log.debug (fun m->m "comitting file write: %s" path)
				| _ -> () in
			return !result
		)

	let write_file fs path contents =
		write_file_s fs path (Lwt_stream.of_list [`Output contents])

	let read_file_s = fun fs path consumer ->
		locked_atomic_read_exn fs path (fun path ->
			Log.debug (fun m->m "Reading file stream: %s" path);
			let offset = ref 0 in
			let max_chunk_size = 4096 in
			consumer (Lwt_stream.from (fun () ->
				let rv = ref "" in
				let chunk_size = ref 0 in
				lwt chunks = Common.read fs path !offset max_chunk_size in
				let () = match chunks with
				(* TODO: mutate `rv` rather than allocating each chunk *)
					| `Ok chunks ->
						chunks |> List.iter (fun chunk ->
							let chunk = Cstruct.to_string chunk in
							chunk_size := !chunk_size + String.length chunk;
							rv := (!rv ^ chunk)
						);
						Log.debug (fun m->m "read %d chunks from %s[%d] => %db" (List.length chunks) path !offset !chunk_size);
						offset := !offset + !chunk_size
					| `Error e -> fail "read" e
				in
				return (
					if !chunk_size = 0 then None else (Some !rv)
				)
			))
		)

	let read_file fs path =
		read_file_s fs path (fun stream -> Lwt_stream.fold (^) stream "")

end
