open Braid

let rec parse_nombre = parser
    | [< ''-'; rest >] -> - (parse_nombre_positif 0 rest)
    | [< nombre = (parse_nombre_positif 0) >] -> nombre
and parse_chiffre = parser
    | [< ' ('0'..'9') as chiffre >] -> (int_of_char chiffre) - (int_of_char '0')
and parse_nombre_positif n = parser
    | [< k = parse_chiffre; rest >] -> parse_nombre_positif (n*10 + k) rest
    | [< '',' >] -> n
                    
let rec parse l = parser
    | [< a = parse_nombre; reste >] -> parse (a::l) reste
    | [< >] -> l

let () =
    while true do
        print_string "> ";
        let input = Stream.of_string (read_line () ^ ",") in
        let list = List.rev (parse [] input) in
        Braid_Print.display (make_braid list);
        Graphics.read_key ();
        Graphics.close_graph ();
    done
