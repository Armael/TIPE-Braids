open Braid;;
open Canonical;;
open Hack;;

(*
let n = 80 and l = 12;;
*)

let n = 20 and l = 10;;

let put_delta d b = {bpl_size = b.bpl_size; delta_power = d; permlist = b.permlist};;

let test () = 
  let pub =  (canonical_form (random_braid n l)) in
  let conjugateur =  (canonical_form (random_braid n l)) in
  let conjugue = canonicize (conjugate pub conjugateur) in

  Printf.printf "Public : ";
  print_braid_permlist pub;
  Printf.printf "\nConjugué : ";
  print_braid_permlist conjugue;
  Printf.printf "\nConjugateur : ";
  print_braid_permlist conjugateur;
  Printf.printf "\n";
  flush stdout;

  try let guessed_priv = try_hack pub conjugue in
      if permlists_equal guessed_priv conjugateur then (
	Printf.printf "\nClef devinée : ";
	print_braid_permlist (canonicize guessed_priv);
	Printf.printf "\n"
      ) else (
	Printf.printf "Mauvaise clef\n"
      )
  with
      Failure s -> Printf.printf "Échec de l'attaque";

  Printf.printf "\n\n";;

let () = 
  for i = 0 to 100 do
    test ()
  done
