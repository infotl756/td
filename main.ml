let produit_cartesien a b = 
    let rec aux acc1 acc2 l =
        match (acc1, acc2) with
            | ([], t::q) -> aux a q l
            | (t::q, []) -> l
            | ([],[]) -> l 
            | (t1::q1, t2::q2) -> aux q1 (t2::q2) ((t1, t2)::l)
    in aux a b [];;


let iteration_droite f l b = 
    match l with
        |[] -> b
        |t :: q -> 
            let rec aux acc l = match l with
                | [] -> acc
                | t::q -> aux (t*(f acc)) q
            in aux (t*(f b)) q;;

let somme = iteration_droite (+) l 0;;

let iteration_gauche f l a =
    match l with
        |[] -> a
        |t :: q -> 
            let rec aux acc l = match l with
                | [] -> acc
                | t::q -> aux (f acc t) q
            in aux (f a t) q;;

let ajoute_tete acc t = t::acc;;

let renverser l = iteration_gauche ajoute_tete l [];;

let rec insere x l = match l with
    | [] -> [x]
    | t::q -> 
        if x <= t then x::l
        else t::(insere x q);;

let tri_insertion = iteration_droite insere l [];;
    