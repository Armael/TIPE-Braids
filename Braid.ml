type braid = {word : int list; size : int};;
type permutation = int array

let inv braid = {word = List.map (fun x -> -x) (List.rev braid.word); size = braid.size}

let concat b1 b2 = {word = b1.word @ b2.word; size = max b1.size b2.size}
let (++) = concat;;

let make_braid ?(size = -1) w =
    let s = (if size = -1 then 1 + List.fold_left (fun a x -> max a (abs x)) 0 w
                          else size) in
    {word = w; size = s};;

let braid_to_permut b =
    let permut = Permutations.make_id b.size in
    List.iter (fun x -> Permutations.transpose permut ((abs x)-1) (abs x)) b.word;
    permut;;

let starting_set b = List.map ((+) 1) (Permutations.consecutive_inversions (braid_to_permut b));;
let finishing_set b = starting_set (inv b);;


