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


let rec est_normal p = match p with
| [] -> true
| [t] -> if t = 0. then false 
         else true
| t::q -> est_normal q;;

let dilatation p a = match p with
	let rec aux 
		| [] -> []
		| t::q -> dilatation 

  let somme p1 p2 =
    let rec aux acc l1 l2 = match (l1, l2) with
        | ([], []) -> List.rev acc
        | (t::q, []) -> aux (t::acc) q []
        | ([], t::q) -> aux (t::acc) [] q
        | (t1::q1, t2::q2) -> aux ((t1 +. t2)::acc) q1 q2
    in aux [] p1 p2;;

let evaluation p v =
    match p with
    | [] -> 0.
    | a::q -> a +. v*(evaluation q v);;

let foisXn p n =
	let rec aux l k = match k with
		|0 -> l
		| _ -> aux  (0::l) (k-1)
	in aux (p) n;;

let a = foisXn [1;2;3;4;5;6] 3;;

let separer l = 
	let taille = List.length l in 
	let rec aux l k l1 l2 = match l with
		|[] -> List.rev l2, List.rev l1
		|t::q -> if k >= (taille/2) then aux q (k+1) (t::l1) l2
					else aux q (k+1) l1 (t::l2)
	in aux l 0 [] [];;
	
	
let b = separer [1;2;3;4;5;6;7;8;9];;

let rec karatsuba a b = 
	
