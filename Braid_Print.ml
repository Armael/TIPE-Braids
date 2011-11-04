open Braid

let display b =
    let print_vert () =
        print_string " |" in
    let print_sigma positif = function
        | 1 -> print_string " \\ /"
        | 2 -> if positif then print_string "  / "
                          else print_string "  \\ "
        | 3 -> print_string " / \\" 
        | _ -> print_string " | |" in
    let rec aux = function
        | [] -> print_newline ()
        | x::xs -> for l = 1 to 4 do
                       for i = 0 to (x-1) do
                        print_vert ()
                       done;
                       print_sigma (x>0) l;
                       for i = x+2 to b.size do
                        print_vert ()
                       done;
                       print_newline ();
                   done;
                   aux xs in
    aux b.word


