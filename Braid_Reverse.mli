val split : ?trace:(Braid.braid -> unit) -> Braid.braid -> Braid.braid;;
val reduce : ?trace:(Braid.braid -> unit) -> Braid.braid -> Braid.braid;;
val is_nil : Braid.braid -> bool
