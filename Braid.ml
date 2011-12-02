type braid = {word : int list; size : int}

let inv braid = {word = List.map (fun x -> -x) (List.rev braid.word); size = braid.size}

let concat b1 b2 = {word = b1.word @ b2.word; size = max b1.size b2.size}

let make_braid ?(size = -1) w =
    let s = (if size = -1 then 1 + List.fold_left (fun a x -> max a (abs x)) 0 w
                          else size) in
    {word = w; size = s};;
