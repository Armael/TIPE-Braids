type braid_permlist = {delta_power : int; permlist : Permutation.permutation list};;

let get_permlist_decomposition b = 
    let n = b.size in
    let neg_to_perm x = 
        transpose (make_delta n) (-x-1) (-x) in (* x négatif *)
    let (delta_pow, perm_stack) = 
        List.fold_left (fun (delta_pow, perm_stack) x ->
                        if x > 0 then
                            (delta_pow, 
                            ((transpose (make_id n) (x-1) x), 0)::perm_stack)
                        else
                            (delta_pow - 1,
                            (neg_to_perm x, 0)::
                             (List.map (fun (p, tau_pow) -> (p, tau_pow + 1)) perm_stack))
                   ([], 0) b.word in
    let perm_list =
        List.rev_map (fun (perm, tau_pow) -> if (tau_pow mod 2) = 1
                                                then Permutation.tau perm
                                                else perm)
                     perm_stack in
    {delta_power = delta_pow; permlist = perm_list};;
    
                        
                            
