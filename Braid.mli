type braid = {word : int list; size : int};;
val inv : braid -> braid;;
val concat : braid -> braid -> braid;;
val make_braid : ?size:int -> int list -> braid;;
