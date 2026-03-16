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

let tri_insertion = iteration_droite insere l [];;
    
(*cor*)

let rec echange = function 
	|[] -> []
	|[a] -> [a]
	|a::b::q when a>b -> b::(echange (a::q))
	|a::b::q  -> a::(echange (b::q));;
	
	
let rec tri_bulle l = 
	let l2= echange l in 
		if l= l2 then l else tri_bulle l2;;



let rec minimum = function
	|[] -> raise (Failure "liste vide")
	|[e]-> e
	|t::q -> min t (minimum q);;	

let extraction l m =
	let 
	let rec aux debut fin = match fin with
		|[] -> debut
		|t::q -> 
			if t = m then debut @ q
			else aux (t::debut) q
	in aux [] l;;
	
let tri_selection l =
	let rec aux l_triee reste = match reste with
		|[] -> List.rev l_triee
		|t::q -> let m = (minimum reste) in 
			aux (m::l_triee) (extraction reste m)
	in aux [] l;;
	
let separation l =
	let len = List.length l in
	let rec aux g d = function
		|i when i = len / 2 -> g, d
		|i -> match d with	
			|[] -> raise (Failure "cas impossible")
			|t::q -> aux (t::g) q (i+1)
	in aux [] l 0;;

let fusion l1, l2 =
	let rec aux deb fin =
		
