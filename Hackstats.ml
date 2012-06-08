open Braid;;
open Canonical;;
open Hack;;
open Sys;;

let patterns = [|(45, 3); (50, 5); (70, 7); (90, 12)|];;

let put_delta d b = {bpl_size = b.bpl_size; delta_power = d; permlist = b.permlist};;

let test n l = 
  Printf.printf "%d,%d: " n l;
  
  let pub =  (canonicize (random_braid_sequence n l)) in
  let conjugateur =  (canonicize (random_braid_sequence n l)) in
  let conjugue = canonicize (conjugate pub conjugateur) in

  try let guessed_priv = try_hack pub conjugue in
      if permlists_equal guessed_priv conjugateur then (
        	Printf.printf "1\n"; (* Clé devinée exactement *)
      ) else (
        	Printf.printf "2\n"; (* Clé devinée à Delta² près *)
      )
  with
      Failure s -> Printf.printf "0\n"; (* Clé non trouvée *)
      flush stdout;;

let () = 
  let nb = (if Array.length argv = 1 then 100
           else int_of_string argv.(1)) in
  for i = 0 to Array.length patterns - 1 do
      let (n, l) = patterns.(i) in
          for j = 1 to nb do
            test n l
          done
  done
