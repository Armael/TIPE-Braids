open Canonical;;
open Braid;;

let n = 80 and l = 12;;

let make_pubkey () = canonicize (random_braid_sequence n l);;

let make_alice_privkey () = canonical_form (random_lower_braid n (5*l));;
let make_bob_privkey () = canonical_form (random_upper_braid n (5*l));;

let make_exchange_key pub priv = conjugate pub priv;;

let make_key exch_key priv = conjugate exch_key priv;;

let () = 
    let public = make_pubkey () in
    let alice_priv = make_alice_privkey () in
    let bob_priv = make_bob_privkey () in
    let alice_exch = make_exchange_key public alice_priv in
    let bob_exch = make_exchange_key public bob_priv in
    
    let tA = make_key bob_exch alice_priv in
    let tB = make_key alice_exch bob_priv in

    print_string (if permlists_equal tA tB then "Ok" else "Pas ok");
    print_newline ();;
