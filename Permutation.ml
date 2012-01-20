let transpose permut i j =
    let tmp = permut.(i) in
    permut.(i) <- permut.(j);
    permut.(j) <- temp;;

let consecutive_inversions permut =
    let n = Array.length permut in
    if n <= 1 then [] else (
        let l = ref [] in
        for i=0 to n-2 do
            if permut.(i) > permut.(i+1)
            then l := i::!l;
        done;
        List.rev !l
    );;

let make_id n =
    let id = Array.make n 0 in
    for i = 0 to n-1 do
        id.(i) <- i
    done;
    id;;

let inv permut =
    let n = Array.length permut in
    let inv = Array.make n 0 in
    for i = 0 to n-1 do
        inv.(permut.(i)) <- i
    done;
    inv;;

let compose p1 p2 =
    let n = Array.length p1 in
    let c = Array.make n 0 in
    for i = 0 to n-1 do
        c.(i) <- p1.(p2.(i))
    done;
    c;;

let (<*>) = compose;;
