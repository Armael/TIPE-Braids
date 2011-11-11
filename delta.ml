open Braid

let rec seq a b =
    if a < b then a::(seq (a+1) b)
    else if a > b then a::(seq (a-1) b)
    else [a];;

let rec affiche = function
    | [] -> print_newline ();
    | x::xs -> print_int x; print_char ' '; affiche xs;;

let delta = ref {word = []; size = 1} in
for i = 1 to 6 do
    print_string ("Delta " ^ (string_of_int i));
    print_newline();
    Braid_Print.display !delta;
    delta := {word = !delta.word @ (seq i 1); size = !delta.size + 1};
    Graphics.read_key ();
    Graphics.close_graph ()
done
