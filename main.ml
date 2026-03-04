(* TP Ocaml sur les listes *)

(* 1 *)
let nombre_occurences l x = 
    let rec aux acc l = match l with
        | [] -> acc
        | a::q -> 
            if a = x then
                aux (acc+1) q 
            else
                aux acc q 
    in aux 0 l;;
    
(* 2 - version couteuse *)
let appliquer f l =
    let rec aux acc l = match l with 
        | [] -> acc
        | a::q -> aux (acc @ f(a)) q
    in aux [] l;;

(* 2 - version optimisée, @ parcourt toute la liste à sa gauche d'où (f a) @ acc pour passer de O(n) à O(n^2) *)
let appliquer f l =
    let rec aux acc l = match l with
        | [] -> List.rev acc
        | a::q -> aux (List.rev (f a) @ acc) q
    in aux [] l;;

(* 3 *)
let maximum l = match l with 
    | [] -> raise (Failure "erreur, liste vide")
    | a::q -> 
        let rec aux acc l = match l with
            | [] -> acc
            | a::q -> 
                if a > acc then
                    aux a q
                else 
                    aux acc q
        in aux a l;; 

(* 4 *)
let dernier l = match l with 
    | [] -> raise (Failure "erreur, liste vide")
    | a::q -> 
        let rec aux acc l = match l with
            | [] -> acc
            | a::q -> aux a q
        in aux a l;;

(* 5 *)
let contient e l = match l with 
    | [] -> raise (Failure "erreur, liste vide")
    | a::q -> 
        let rec aux acc l = match l with
            | [] -> acc
            | a::q -> 
                if a = e then
                    aux true q
                else 
                    aux acc q
        in aux false l;;
    
(* 6 *)
let indice e l = 
    let rec aux i l = match l with
        | [] -> raise (Failure "élément non présent")
        | a::q -> 
            if a = e then i
            else aux (i+1) q
    in aux 0 l;;

(* 7 *)
let liste_indice e l =
    let rec aux acc i l = match l with
        | [] -> 
            if acc = [] then raise (Failure "élément non présent")
            else acc
        | a::q -> 
            if a = e then aux (i::acc) (i+1) q
            else aux acc (i+1) q
    in aux [] 0 l;;

(* 8 *)
let nieme n l = 
    let rec aux i l = match l with
        | [] -> raise (Failure "index error")
        | a::q -> 
            if i = n then a
            else aux (i+1) q
    in aux 0 l;;

(* 9 *)
let rec pour_tout p l = match l with
    | [] -> true
    | a::q -> 
        if p a = true then pour_tout p q
        else false;;

(* 10 *)
let rec existe p l = match l with
    | [] -> false
    | a::q -> 
        if p a then true
        else existe p q;;

(* 11 *)
let rec trouver p l = match l with
    | [] -> raise (Failure "Not Found")
    | a::q -> 
        if p a then a 
        else trouver p q;;

(* 12 , attention l'ordre de renvoi est inversé*)
let filtre p l = 
    let rec aux acc l = match l with
        | [] -> acc
        | a::q -> 
            if p a then aux (a::acc) q
            else aux acc q
    in aux [] l;; 

(* 13 , attention l'ordre de renvoi est inversé*)
let partition p l =
    let rec aux (acc1, acc2) l = match l with
        | [] -> (acc1, acc2)
        | a::q -> 
            if p a then aux (a::acc1, acc2) q
            else aux (acc1, a::acc2) q
    in aux ([], []) l;;

(* 14 , ici l'ordre de renvoi n'est pas inversé*)
let separer l =
    let rec aux (acc1, acc2) l = match l with
        | [] -> (List.rev acc1, List.rev acc2)
        | (a,b)::q -> aux ((a::acc1), (b::acc2)) q
    in aux ([], []) l;;

(* 15 *)
let iterees a n f = 
    let rec aux acc l i = 
        if i = n then l
        else aux (f acc) (f acc::l) (i+1)
    in aux a [a] 0;;

(* 16 *)
let entiersC n =
    let rec aux k l = 
        if k = 0 then 0::l
        else aux (k-1) (k::l)
    in aux n [];;

let l1 = entiersC 10;;