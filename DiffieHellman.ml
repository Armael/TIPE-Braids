open Canonical;;
open Braid;;
open Hack;;

let n = 80 and l = 12;;

let make_pubkey () = canonicize (random_braid_sequence n l);;

let make_alice_privkey () = canonical_form (random_lower_braid n (5*l));;
let make_bob_privkey () = canonical_form (random_upper_braid n (5*l));;

let make_exchange_key pub priv = conjugate pub priv;;

let make_key exch_key priv = conjugate exch_key priv;;

let tentative_de_hack pub exch =
  let n = pub.bpl_size in
  try a_algorithm pub exch
  with Failure s -> (
    try a_algorithm (conjugate (delta n) pub) exch
    with Failure s -> raise (Failure "fail")
  )

let () = 
    let public = make_pubkey () in
    let alice_priv = make_alice_privkey () in
    let bob_priv = make_bob_privkey () in
    let alice_exch = make_exchange_key public alice_priv in
    let bob_exch = make_exchange_key public bob_priv in
    
    let tA = make_key bob_exch alice_priv in
    let tB = make_key alice_exch bob_priv in

    print_string (if permlists_equal tA tB then "Clé échangée avec succès" else "Échec");
    print_newline ();
    
(*    try let guessed_priv = tentative_de_hack public alice_exch in
	let guessed_secret = make_key bob_exch guessed_priv in
	if (canonicize guessed_secret) = (canonicize tA) then
	  Printf.printf "Clé secrète découverte avec succès !\n"
	else Printf.printf "Mauvaise clef\n" with
	    Failure s -> Printf.printf "Échec de l'attaque\n";;
*)


