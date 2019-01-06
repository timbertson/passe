type lock
type proof
exception Stale_lock
val create : unit -> lock
val is_empty : lock -> bool
val use : ?proof:proof -> lock -> (proof -> 'a Lwt.t) -> 'a Lwt.t

module Map (Ord: OrderedType.S) ( ) : sig
	val acquire : Ord.t -> ?proof:proof -> (proof -> 'a Lwt.t) -> 'a Lwt.t
end

