open Braid

let delta_array = Array.make 100 None;;

delta_array.(0) <- Some {word = []; size = 0};;

let rec seq a b =
    if a < b then a::(seq (a+1) b)
    else if a > b then a::(seq (a-1) b)
    else [a];;

let rec delta n = match delta_array.(n-1) with
    | Some d -> d
    | None -> {word = (delta (n-1)).word @ (seq (n-1) 1); size = n};;

let rec affiche = function
    | [] -> print_newline ();
    | x::xs -> print_int x; print_char ' '; affiche xs;;

(*let print_delta () = 
    for i = 1 to 6 do
        print_string ("Delta " ^ (string_of_int i));
        print_newline();
        Braid_Print.display (delta i);
        Graphics.read_key ();
        Graphics.close_graph ()
    done;;*)
    
    
(*    let b = {word = [1;-2;3;-1]; size = 4};; *)
    
    Braid_Print.display (delta 4);;
