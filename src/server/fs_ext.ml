open Passe
open Common
open Lwt
open Result

(* FS plus helpers *)
module type Augmented = sig
	include Mirage_fs.S

	val as_write_error : error -> write_error

	val mkdir_p : t -> string -> (unit, write_error) result Lwt.t

	val ensure_empty : t -> string -> (unit, write_error) result Lwt.t

	val ensure_exists : t -> string -> (unit, write_error) result Lwt.t

	val destroy_if_exists : t -> string -> (unit, write_error) result Lwt.t
end

module AtomicTypes = struct
	type write_commit = [ `Commit | `Rollback ]
end

module type AtomicSig = functor(Fs:Augmented) -> sig
	include module type of AtomicTypes

	val with_writable: Fs.t -> Path.full
		-> (Path.full -> (write_commit, Fs.write_error) result Lwt.t)
		-> (unit, Fs.write_error) result Lwt.t

	val readable:  Fs.t -> Path.full -> (Path.full, Fs.error) result Lwt.t
end

module Augment (Fs:Mirage_fs.S) : (Augmented with type t = Fs.t) = struct
	include Fs

	(* this is inelegant... *)
	let as_write_error (err:error) : write_error = match err with
		(* should be able to just upcast this... *)
		| `Is_a_directory -> `Is_a_directory
		| `No_directory_entry -> `No_directory_entry
		| `Not_a_directory -> `Not_a_directory
		| _ -> failwith "Unknown error type"

	(* returns whether the file was created *)
	let _ensure_exists fs path =
		stat fs path >>= (function
			| Ok _ -> return (Ok false)
			| Error `No_directory_entry ->
				create fs path |> Lwt_r.map (fun () -> true)
			| Error err -> return (Error (err |> as_write_error))
		)

	let ensure_exists fs path =
		_ensure_exists fs path |> Lwt_r.map (fun (_created:bool) -> ())

	let ensure_empty fs path =
		(* Note: not atomic *)
		_ensure_exists fs path |> Lwt_r.bind (fun created ->
			if not created then begin
				(* recreate to truncate *)
				destroy fs path |> Lwt_r.bind (fun () ->
					create fs path
				)
			end else return (Ok ())
		)
	
	let destroy_if_exists fs path =
		match%lwt destroy fs path with
			| Ok _ | Error `No_directory_entry -> return (Ok ())
			| Error _ as e -> return e

	let mkdir_p fs =
		let rec mkdir_p path = mkdir fs path |> Lwt.bindr (function
			| Error `No_directory_entry ->
				let parent = (Filename.dirname path) in
				assert (path <> parent);
				mkdir_p parent |> Lwt_r.bind (fun () -> mkdir_p path)
			| other -> return other
		) in
		mkdir_p
end

