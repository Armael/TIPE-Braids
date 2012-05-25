open Canonical;;
module P = Permutation;;

(* Retourne le inf / sup d'une tresse sous forme canonique
*)

let inf b = b.delta_power;;
let sup b = b.delta_power + (List.length b.permlist);;

(* Tresses de base 
*)

let id = fun x -> x;;

(* Retourne le dernier element d'une liste
*)
let rec last = function
  | [] -> raise (Failure "last")
  | [x] -> x
  | x::xs -> last xs;;

(* Transforme une permutation represantant une tresse simple _differente de delta
   et de l'identité _ en la braid_permlist associée
*)
let p2b perm = {bpl_size = Array.length perm; delta_power = 0; permlist = [perm]};;

let cycling1 n w =
  let g = (delta n) <*> 
    (inverse (p2b ((if (inf w) mod 2 <> 0 then P.tau else id) (List.hd w.permlist)))) in
  (g, g <*> w <*> (inverse g));;

let cycling2 n w =
  let g = (delta n) <*> 
    (inverse (p2b ((if (inf w) mod 2 <> 0 then P.tau else id) (List.hd w.permlist)))) in
  let rec cycle r a = function
    | [] -> [(if ((r+1) mod 2 <> 0) then P.tau else id) a]
    | x::xs -> (P.tau x)::(cycle r a xs) in
  (g, {bpl_size = w.bpl_size; delta_power = w.delta_power; 
       permlist = (if w.permlist = [] then [] else (cycle w.delta_power (List.hd w.permlist) (List.tl w.permlist)))});;

let guess_permutation v w =
  let n = v.bpl_size in
  let tau = P.make_id n in
  let chi = Array.make n false in
  let perm_v = braid2perm v and perm_w = braid2perm w in
  let r = ref 0 and s = ref 0 in
  for i = n-1 downto 0 do
    r := i; s := i; 
    while chi.(!r) = false do
      chi.(!r) <- true;
      r := perm_v.(!r);
      s := perm_w.(!s);
      if !r <> !s then
	tau.(!r) <- !s
    done;
  done;
  let xi = Array.make n false in
  for i = n-1 downto 0 do
    if xi.(i) = false then (
      xi.(i) <- true;
      r := i;
      while xi.(tau.(!r)) = false && tau.(!r) <> !r do
	r := tau.(!r);
	xi.(!r) <- true
      done;
      tau.(!r) <- i
    )
  done;
  if P.is_id tau then {bpl_size = n; delta_power = 0; permlist = []}
  else if P.is_delta tau then {bpl_size = n; delta_power = 1; permlist = []}
  else {bpl_size = n; delta_power = 0; permlist = [tau]};;

exception Echec;;

let a_algorithm v w =
  let n = v.bpl_size in
  let a = ref (empty n) in
  let cv = canonicize v and nw = ref (canonicize w) in
  while (inf !nw) < (inf cv) do
    let c = cycling1 n !nw in
    a := (fst c) <*> !a;
    nw := snd c;
    nw := canonicize !nw;
  done;
  while (sup !nw) > (sup cv) do
    let g = p2b (last !nw.permlist) in
    a := g <*> !a;
    nw := g <*> !nw <*> (inverse g);
    nw := canonicize !nw;
  done;
  let m = guess_permutation cv !nw in
  a := m <*> !a;
  nw := m <*> !nw <*> (inverse m);
  if permlists_equal cv !nw then !a else raise Echec;;

let try_hack pub exch (* conjugué *) =
  let n = pub.bpl_size in
  try a_algorithm pub exch
  with Echec -> (
    try let res2 = a_algorithm (conjugate pub (delta n)) exch in
	delta n <*> res2
    with Echec -> raise (Failure "fail")
       | Failure s -> Printf.printf "Error : %s\n" s; raise (Failure "epic fail")
  );;
