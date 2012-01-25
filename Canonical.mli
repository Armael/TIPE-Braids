open Braid;;

type braid_permlist = {delta_power : int; permlist : Permutation.permutation list};;
val get_permlist_decomposition : braid -> braid_permlist;;
val make_left_weighted : Permutation.permutation list -> Permutation.permutation list;;
val canonicize : braid_permlist -> braid_permlist;;
val canonical_form : braid -> braid_permlist;;
val compare_braids : braid -> braid -> bool;;
val compare_permlists : braid_permlist -> braid_permlist -> bool;;

val product : braid_permlist -> braid_permlist -> braid_permlist;;
val (<*>) : braid_permlist -> braid_permlist -> braid_permlist;;
val inverse : braid_permlist -> braid_permlist;;
val conjugate : braid_permlist -> braid_permlist -> braid_permlist;;

val random_braid_sequence : int -> int -> braid_permlist;;

