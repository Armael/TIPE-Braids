type braid = {word : int list; size : int}

let inv braid = {word = List.map (fun x -> -x) (List.rev braid.word); size = braid.size}

let concat b1 b2 = {word = b1.word @ b2.word; size = max b1.size b2.size}
