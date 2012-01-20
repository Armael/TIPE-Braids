type braid = {word : int list; size : int};;
type permutation = int array;;
val inv : braid -> braid;;
val concat : braid -> braid -> braid;;
val (++) : braid -> braid -> braid;;
val make_braid : ?size:int -> int list -> braid;;
val braid_to_permut : braid -> permutation;;
