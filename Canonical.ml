module P = Permutation;;

type braid_permlist = {delta_power : int; permlist : P.permutation list};;

(* Convert a braid word to a permlist *)
let get_permlist_decomposition (b : Braid.braid) = 
    let n = b.Braid.size in
    (* Calcule Delta * sigma^-1 que l'on sait être un facteur canonique *)
    let neg_to_perm x =
        let perm = P.make_delta n in
        P.transpose perm (-x-1) (-x); (* x négatif *)
        perm in
    let (delta_pow, perm_stack) = 
        List.fold_left (fun (delta_pow, perm_stack) x ->
                          if x > 0 then
                            (delta_pow, 
                            (P.make_transpose (x-1) x n, 0)::perm_stack)
                          else
                            (delta_pow - 1,
                             (neg_to_perm x, 0)::
                             (List.map (fun (p, tau_pow) -> (p, tau_pow + 1)) perm_stack)))
                       (0, []) b.Braid.word in
    let perm_list =
        List.rev_map (fun (perm, tau_pow) -> if (tau_pow mod 2) = 1
                                                then P.tau perm
                                                else perm)
                     perm_stack in
    {delta_power = delta_pow; permlist = perm_list};;

(* Conversion functions to left-weighted/canonical/normal form *)

let rec make_left_weighted = function
  | p1::p2::q ->
      let s2 = P.starting_set p2 and f1 = P.finishing_set p1 in
        if s2 = [] (* p2 = neutre *) then make_left_weighted (p1::q)
        else if f1 = [] (* p1 = e *) then make_left_weighted (p2::q)
        else (
          match P.set_difference s2 f1 with
            | [] -> p1::make_left_weighted (p2::q)
            | i::_ -> let p1' = Array.copy p1 in
                      P.transpose p1' (i-1) i;
                      let p2' = P.compose p2 (P.make_transpose (i-1) i (Array.length p2)) in
                      make_left_weighted (p1'::p2'::q)
        )
  | pl -> pl;;
  
let canonicize bpl =
  let dp = ref bpl.delta_power 
  and pl = ref (make_left_weighted bpl.permlist)
  and ok = ref false in
  while not !ok && !pl <> [] do
    if P.is_delta (List.hd !pl) then (
      dp := !dp + 1;
      pl := List.tl !pl
    ) else (
      ok := true
    )
  done;
  {delta_power = !dp; permlist = !pl};;

let canonical_form b = canonicize (get_permlist_decomposition b);;

(* Equality tests *)
let compare_braids b1 b2 = (canonical_form b1) = (canonical_form b2);;
let compare_permlists bpl1 bpl2 = (canonicize bpl1) = (canonicize bpl2);;

(* Algebraic operations on permlists *)

let product a b =
  {delta_power = a.delta_power + b.delta_power;
   permlist = (if a.delta_power mod 2 = 1 then List.map P.tau a.permlist else a.permlist)
              @ b.permlist };;

let (<*>) = product;;

let inverse b = 
  let l = List.length b.permlist and q = b.delta_power in
  {
    delta_power = - q - l;
    permlist = snd (List.fold_left (fun (parity, acc) perm ->
                                      ((parity + 1) mod 2,
                                       (if parity = 0 then perm::acc else (P.tau perm)::acc)))
                                    ((q + l) mod 2, []) b.permlist)
  };;


let conjugate a b = b <*> a <*> (inverse b);;

(* Appeler avec l = DOUZE, n de l'ordre de 10^(1 ou 2) *)
let random_braid_sequence n l =
  let rec loop acc = function
    | 0 -> acc
    | i -> loop (P.random_permutation n :: acc) (i-1)
  in
  {delta_power = l-(Random.int (3*l)); (* tout à fait arbitraire *)
   permlist = loop [] l};;

let print_braid_permlist bpl =
  Printf.printf "(%d | " bpl.delta_power;
  List.iter (fun p -> P.print_permutation p; print_string " ") bpl.permlist;
  print_string ")";;
