type lock
type proof
exception Stale_lock
val create : unit -> lock
val is_empty : lock -> bool
val use : ?proof:proof -> lock -> (proof -> 'a Lwt.t) -> 'a Lwt.t
