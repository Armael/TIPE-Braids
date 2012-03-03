type braid = {word : int list; size : int};;
val inv : braid -> braid;;
val concat : braid -> braid -> braid;;
val (++) : braid -> braid -> braid;;
val make_braid : ?size:int -> int list -> braid;;
val random_braid : int -> int -> braid;;
val random_lower_braid : int -> int -> braid;;
val random_upper_braid : int -> int -> braid;;
