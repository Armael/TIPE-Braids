type permutation = int array;;

val transpose : permutation -> int -> int -> unit;;
val consecutive_inversions : permutation -> int list;;
val make_id : int -> permutation;;
val make_transpose : int -> int -> int -> permutation;;
val make_delta : int -> permutation;;
val is_delta : permutation -> bool;;
val inv : permutation -> permutation;;
val compose : permutation -> permutation -> permutation;;
val tau : permutation -> permutation;;
val braid_to_permut : Braid.braid -> permutation;;
val starting_set : permutation -> int list;;
val finishing_set : permutation -> int list;;
val is_subset : int list -> int list -> bool;;
val set_difference : int list -> int list -> int list;;
val shuffle : 'a array -> 'a array;;
val random_permutation : int -> permutation;;
