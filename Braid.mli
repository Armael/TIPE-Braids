type braid = {word : int list; size : int};;
val inv : braid -> braid;;
val concat : braid -> braid -> braid;;
val (++) : braid -> braid -> braid;;
val make_braid : ?size:int -> int list -> braid;;
val random_braid : int -> int -> braid;;
val random_LBn_braid : int -> int -> braid;;
val random_UBn_braid : int -> int -> braid;;
