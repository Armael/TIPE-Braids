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

(* Transforme une permutation represantant une tresse simple _differente de delta_
   en la braid_permlist associee
*)
let p2b perm = {bpl_size = Array.length perm; delta_power = 0; permlist = [perm]};;

let guess_permutation v w =
  let n = v.bpl_size in
  let tau = P.make_id n in
  let chi = Array.make n false in
  let pi_v = braid2perm v and pi_w = braid2perm w in
  let r = ref 0 and s = ref 0 in
  for i = n-1 downto 0 do
    r := i; s := i; 
    while chi.(!r) = false do
      chi.(!r) <- true;
      r := pi_v.(!r);
      s := pi_w.(!s);
      if !r <> !s then
	tau.(!r) <- !s
    done;
  done;
  let xi = Array.make n false in
  for i = 0 to n-1 do
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

let a_algorithm v w =
  let n = v.bpl_size in
  let a = ref (empty n) in
  let nv = ref (canonicize v) and nw = ref (canonicize w) in
  while (inf !nw) < (inf !nv) do
    let g = (delta n) <*> 
            (inverse (p2b ((if (inf !nw) mod 2 <> 0 then P.tau else id) (List.hd !nw.permlist)))) in
    a := g <*> !a;
    nw := g <*> !nw <*> (inverse g);
    nw := canonicize !nw
  done;
  while (sup !nw) > (sup !nv) do
    let g = p2b (last !nw.permlist) in
    a := g <*> !a;
    nw := g <*> !nw <*> (inverse g);
    nw := canonicize !nw
  done;
  let m = guess_permutation !nv !nw in
  a := m <*> !a;
  nw := m <*> !nw <*> (inverse m);
  if !nv = !nw then !a else raise (Failure "failed");;

