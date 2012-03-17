open Braid;;

type braid_permlist = {bpl_size : int; delta_power : int; permlist : Permutation.permutation list};;

val delta : int -> braid_permlist;;
val empty : int -> braid_permlist;;

val get_permlist_decomposition : braid -> braid_permlist;;
val make_left_weighted : Permutation.permutation list -> Permutation.permutation list;;
val canonicize : braid_permlist -> braid_permlist;;
val canonical_form : braid -> braid_permlist;;
val braids_equal : braid -> braid -> bool;;
val permlists_equal : braid_permlist -> braid_permlist -> bool;;

val product : braid_permlist -> braid_permlist -> braid_permlist;;
val (<*>) : braid_permlist -> braid_permlist -> braid_permlist;;
val inverse : braid_permlist -> braid_permlist;;
val conjugate : braid_permlist -> braid_permlist -> braid_permlist;;

val braid2perm : braid_permlist -> Permutation.permutation;;

val random_braid_sequence : int -> int -> braid_permlist;;

val print_braid_permlist : braid_permlist -> unit;;
