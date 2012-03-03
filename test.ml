open Braid;;
open Canonical;;

let print_int_list = List.iter (fun x -> Printf.printf "%d " x);;

let print_int_array_list = List.iter
    (fun a -> Printf.printf "[ ";
              Array.iter (fun x -> Printf.printf "%d " x) a;
              Printf.printf " ] ");; 

let _ =
(*    let b = random_braid_sequence 12 80 in
    let bc = canonicize b in 
    let i = inverse b in
    let p = b <*> i in
    let pc = canonicize p in
    print_braid_permlist pc *)


    let b = random_braid 80 12 in
    let i = inv b in
    let p = i ++ b in
    let pc = canonical_form p in
    print_string (if Braid_Reverse.is_nil p then "OK" else "Pas ok");
    print_newline ();

    print_braid_permlist pc;;
