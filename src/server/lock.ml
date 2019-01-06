open Passe
module Log = (val Logging.log_module "lock")

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
			let%lwt () = Lwt_mutex.lock lock in
			let proof = { lock; valid = ref true; } in
			(try%lwt
				fn proof
			with e -> raise e
			) [%lwt.finally
				proof.valid := false;
				Lwt_mutex.unlock lock;
				Lwt.return_unit
			]
		)


module Map (Ord: OrderedType.S) ( ) = struct
	module LockMap = Map.Make(Ord)

	let locks = ref LockMap.empty

	let acquire key ?proof fn =
		let lock =
			try LockMap.find key !locks
			with Not_found -> begin
				let lock = create () in
				locks := LockMap.add key lock !locks;
				lock
			end in
		(try%lwt
			Log.debug (fun m->m "acquiring lock for %a" Ord.pp key);
			use ?proof lock (fun proof ->
				Log.debug (fun m->m "acquired lock for %a" Ord.pp key);
				fn proof
			)
		with e -> raise e
		) [%lwt.finally
			Log.debug (fun m->m "released lock for %a; is_empty = %b" Ord.pp key (is_empty lock));
			if (is_empty lock) then (
				locks := LockMap.remove key !locks
			);
			Lwt.return_unit
		]
end
