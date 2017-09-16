type lock = Lwt_mutex.t
type proof = {
	lock: lock;
	valid: bool ref;
}
exception Stale_lock

let create () = Lwt_mutex.create ()
let is_empty lock = Lwt_mutex.is_empty lock

let valid ~lock proof =
	!(proof.valid) && lock == proof.lock

let use ?proof lock fn =
	match proof with
		| Some proof ->
			if valid ~lock proof
				then fn proof
				else raise Stale_lock
		| None -> (
			let proof = { lock; valid = ref true; } in
			let%lwt () = Lwt_mutex.lock lock in
			(try%lwt
				fn proof
			with e -> raise e
			) [%lwt.finally
				proof.valid := false;
				Lwt_mutex.unlock lock;
				Lwt.return_unit
			]
		)
