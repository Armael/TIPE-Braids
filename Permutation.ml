type permutation = int array;;

let transpose permut i j =
    let tmp = permut.(i) in
    permut.(i) <- permut.(j);
    permut.(j) <- tmp;;

(* La fonction renvoie une liste _triée_
   des inversions *)
let consecutive_inversions permut =
    let n = Array.length permut in
    if n <= 1 then [] else (
        let l = ref [] in
        for i=0 to n-2 do
            if permut.(i) > permut.(i+1)
            then l := i::!l;
        done;
        List.rev !l
    );;

let make_id n =
    let id = Array.make n 0 in
    for i = 0 to n-1 do
        id.(i) <- i
    done;
    id;;

let make_transpose i j n =
    let t = make_id n in
    transpose t i j;
    t;;

let make_delta n =
    let delta = Array.make n 0 in
    for i = 0 to n-1 do
        delta.(i) <- n-1-i
    done;
    delta;;

let is_delta p =
  let n = Array.length p in
  let ok = ref true and i = ref 0 in
  while !ok && !i < n do
    ok := (p.(!i) = n - 1 - !i);
    i := !i+1
  done;
  !ok;;

let inv permut =
    let n = Array.length permut in
    let inv = Array.make n 0 in
    for i = 0 to n-1 do
        inv.(permut.(i)) <- i
    done;
    inv;;

let compose p1 p2 =
    let n = Array.length p1 in
    let c = Array.make n 0 in
    for i = 0 to n-1 do
        c.(i) <- p1.(p2.(i))
    done;
    c;;

(* Conjugué par Delta;
tau(sigma_i) = sigma_(n-i) *)
let tau p =
    let n = Array.length p in 
    let q = Array.make n 0 in
    for i = 0 to n - 1 do
        q.(n-1-i) <- n-1-p.(i)
    done;
    q;;

let braid_to_permut (b : Braid.braid) =
    let permut = make_id b.Braid.size in
    List.iter (fun x -> transpose permut ((abs x)-1) (abs x)) b.Braid.word;
    permut;;

(* Les listes retournées sont triées pour les deux fonctions ci-dessous *)
let starting_set p = List.map ((+) 1) (consecutive_inversions p);;
(* On a bien S(inv de permutation) = F(permutation) à la place d'utiliser
le rev de la tresse correspondante *)
let finishing_set p = starting_set (inv p);;

(* Les listes doivent être triées *)
let rec is_subset e f = match (e, f) with
    | ([], _) -> true
    | (_, []) -> false
    | (x::xs, y::ys) -> if x < y then false
                        else if x = y then is_subset xs ys
                             else is_subset (x::xs) ys;; 

let rec set_difference e f = match (e, f) with
    | ([], _) -> []
    | (_, []) -> e
    | (x::xs, y::ys) -> if x < y then x::(set_difference xs (y::ys))
                        else if x = y then set_difference xs ys
                             else set_difference (x::xs) ys;; 


(* Mélange aléatoire d'un tableau avec l'algo de Knuth-Fisher-Yates,
   appliqué à la génération d'une permutation aléatoire *)

(* mélange sur place, modifie le tableau *)
let shuffle t =
  Random.self_init ();
  let swap i j = let temp = t.(i) in
                 t.(i) <- t.(j);
		 t.(j) <- temp
  in
  let n = Array.length t in
  for i = n-1 downto 0 do
    swap i (Random.int (i+1));
  done;
  t;;

let random_permutation n = shuffle (make_id n);;

let print_permutation p =
  print_string "[";
  print_int p.(0);
  for i=1 to Array.length p - 1 do
    Printf.printf " %d" p.(i);
  done;
  print_string "]";;
