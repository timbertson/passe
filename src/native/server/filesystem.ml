open Lwt
module type FS = sig
	include V1_LWT.FS with type page_aligned_buffer = Cstruct.t
end

module type Sig = sig
	include FS

	type exception_reason =
		| ENOENT of string
		| FS_ERROR of string

	exception Error of exception_reason

	type 'a result = [ `Ok of 'a | `Error of error ]

	val fail : string -> error -> 'a

	val unwrap : string -> 'a result -> 'a

	val unwrap_lwt : string -> 'a result Lwt.t -> 'a Lwt.t

	(* shouldn't this be in the mirage FS signature? *)
	val error_message : error -> string

	val destroy_if_exists : t -> string -> unit result Lwt.t
	val write_file_s : t -> string -> string Lwt_stream.t -> unit Lwt.t
	val write_file : t -> string -> string -> unit result Lwt.t
	val read_file_s : t -> string -> string Lwt_stream.t
	val read_file : t -> string -> string Lwt.t
	val rename : t -> string -> string -> unit Lwt.t
end

module Make (Fs: FS)(Logging:Passe.Logging.Sig) = struct
	let log = Logging.get_logger "fs"

	include Fs
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
		log#debug "%s operation failed: %s" desc errmsg;
		raise (Error (match e with
			| `No_directory_entry (parent,ent) -> ENOENT (Filename.concat parent ent)
			| e -> FS_ERROR errmsg
		))

	let unwrap desc op =
		match op with
		| `Ok x -> x
		| `Error e -> fail desc e

	let unwrap_lwt desc = Lwt.map (unwrap desc)

	let ensure_file_exists fs path =
		match_lwt stat fs path with
			| `Ok _ -> return_unit
			| `Error (`No_directory_entry (_,_)) -> create fs path |> unwrap_lwt "create"
			| `Error e -> fail "stat" e
	
	let destroy_if_exists fs path =
		match_lwt destroy fs path with
			| `Ok _ | `Error (`No_directory_entry (_,_)) -> return (`Ok ())
			| `Error _ as e -> return e

	let write_file_s fs path stream =
		log#debug "Writing file stream: %s" path;
		lwt () = ensure_file_exists fs path in
		let offset = ref 0 in
		stream |> Lwt_stream.iter_s (fun chunk ->
			let size = String.length chunk in
			let prev_offset = !offset in
			offset := !offset + size;
			log#trace "writing chunk of len %d to %s at offset %d" size path prev_offset;
			unwrap_lwt "write" (write fs path prev_offset (Cstruct.of_string chunk))
		)

	let write_file fs path contents =
		lwt () = ensure_file_exists fs path in
		log#debug "Writing file: %s" path;
		let buf = Cstruct.of_string contents in
		write fs path 0 buf

	let read_file_s fs path =
		log#trace "Reading file stream: %s" path;
		let offset = ref 0 in
		let max_chunk_size = 4096 in
		Lwt_stream.from (fun () ->
			let rv = ref "" in
			let chunk_size = ref 0 in
			lwt chunks = read fs path !offset max_chunk_size in
			let () = match chunks with
			(* TODO: mutate `rv` rather than allocating each chunk *)
				| `Ok chunks ->
					log#trace "read %d chunks from %s[%d]" (List.length chunks) path !offset;
					chunks |> List.iter (fun chunk ->
						let chunk = Cstruct.to_string chunk in
						chunk_size := !chunk_size + String.length chunk;
						rv := (!rv ^ chunk)
					);
					offset := !offset + !chunk_size
				| `Error e -> fail "read" e
			in
			return (
				if !chunk_size = 0 then None else (Some !rv)
			)
		)

	let read_file fs path =
		let stream = read_file_s fs path in
		Lwt_stream.fold (^) stream ""

end
