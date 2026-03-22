let rec est_normal p = match p with
| [] -> true
| [t] -> if t = 0. then false 
         else true
| t::q -> est_normal q

let dilatation p a =
  let rec aux acc l = match l with
    | [] -> List.rev acc
    | t::q -> aux ((a *. t) :: acc) q
  in aux [] p

  let somme p1 p2 =
    let rec aux acc l1 l2 = match (l1, l2) with
        | ([], []) -> List.rev acc
        | (t::q, []) -> aux (t::acc) q []
        | ([], t::q) -> aux (t::acc) [] q
        | (t1::q1, t2::q2) -> aux ((t1 +. t2)::acc) q1 q2
    in aux [] p1 p2

let evaluation p v =
    let rec aux acc l = match l with
        | [] -> acc
        | t::q -> 

let tri_insertion = iteration_droite insere l [];;
    