type braid = {word : int list; size : int}

let inv braid = {word = (List.rev braid.word); size = braid.size}
