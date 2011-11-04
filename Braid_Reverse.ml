open Braid

let split b =
    let passe = function
        | (b, []) -> (b, []) (* le premier élément du couple indique si on a fait une substitution *)
        | (b, x::[]) -> (b, [x])
        | (b, i::j::xs) -> if i*j < 0 && i < 0 then
                      (* on peut faire une substitution, mais pas d'appel récursif car on n'en 
                      fait qu'une par passe [pour l'instant, peut être amélioré :-°] *)
                        (true, 
                        if i = -j then 
                            xs
                        else if abs (-i-j) = 1 then
                            j::-i::-j::i::xs
                        else 
                            j::i::xs )
                      else
                        let s = passe xs in (* pas de substitution, on continue *)
                        (fst s, i::j::(snd s) in 
    let stop = ref false in
    while not !stop do
        stop := true;
        
