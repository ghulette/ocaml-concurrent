type 'a t
exception IVar_full
val create : unit -> 'a t
val write : 'a t -> 'a -> unit
val read : 'a t -> 'a
