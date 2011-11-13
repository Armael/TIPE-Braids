open Braid

let split ?(trace = (fun _ -> ())) braid =
    let rec passe = function
        | [] -> (false, []) (* le premier élément du couple indique si on a fait une substitution *)
        | x::[] -> (false, [x])
        | i::j::xs -> if i*j < 0 && i < 0 then
                      (* on peut faire une substitution, mais pas d'appel récursif car on n'en 
                      fait qu'une par passe [pour l'instant, peut être amélioré :-°] *)
                        (true,
                         (if i = -j then 
                           xs
                         else if abs (-i-j) = 1 then
                           j::-i::-j::i::xs
                         else 
                           j::i::xs) )
                      else
                        let s = passe (j::xs) in (* pas de substitution, on continue *)
                          (fst s, i::(snd s)) in 
    trace braid;
    let p = ref (passe braid.word) in
    while fst !p do
        trace {word = (snd !p); size = braid.size};
        p := passe (snd !p)
    done;
    {word = snd !p; size = braid.size};;

let rev_splitted braid = 
    let rev_w = List.map (fun x -> -x) (List.rev braid.word) in
        {word = rev_w; size = braid.size};;

let reduce ?(trace = (fun _ -> ())) braid = 
    split ~trace:trace (rev_splitted (split ~trace:trace braid));;

let is_nil braid = (reduce braid).word = [];;
