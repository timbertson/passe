(* abstract type (like KV) abstracting ove Cloud_storage, Fs & KV *)
module Core = struct
	type error = [
		| `Invalid (* will never succeed *)
		| `Failed of string (* possibly-transitory error *)
	]
	let string_of_error : error -> string = Obj.magic

	type write_instruction = [ `Output of string | `Rollback ]
end

module type Sig = sig
	include module type of Core
	type t
	module Path: sig
		type t
		val pp : t Fmt.t
		val make : string list -> (t, error) result
	end

	(* simple read/write *)
	val read : t -> Path.t -> (string option, error) result Lwt.t
	val write : t -> Path.t -> string -> (unit, error) result Lwt.t

	(* advanced read/write *)
	val read_for_writing : t -> Path.t -> (Lock.proof ->
		((string, error) result Lwt_stream.t option, error) result Lwt.t -> 'a Lwt.t) -> 'a Lwt.t

	val read_s : t -> Path.t -> ((string, error) result Lwt_stream.t -> ('a, error) result Lwt.t) -> ('a option, error) result Lwt.t

	val write_s : t -> Path.t -> ?proof:Lock.proof -> write_instruction Lwt_stream.t -> (unit, error) result Lwt.t

	val delete : t -> Path.t -> (unit, error) result Lwt.t
	val connect : string -> t
end

module Of_fs(Fs: Filesystem.Sig) : Sig = struct
	include Core
	type t = unit
	module Path = struct
		type t = unit
		let pp = Fmt.nop
		let make = Obj.magic
	end

	let read_for_writing = Obj.magic
	let read_s = Obj.magic
	let read = Obj.magic

	let write_s = Obj.magic
	let write = Obj.magic
	(* TODO: ensure_exists before write *)
			(* let dbdir = db_path_for ?user:None new_data_root |> R.assert_ok string_of_invalid_path in *)
			(* Kv.stat db dbdir |> Lwt.bindr (function *)
			(* 	| Ok _ -> return (Ok ()) *)
			(* 	| Error `No_directory_entry -> Db.mkdir db dbdir *)
			(* 	| Error e -> return (Error (e |> Db.as_write_error)) *)
			(* ) *)

	let delete = Obj.magic
	let connect = Obj.magic
end
