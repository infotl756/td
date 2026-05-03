type 'a ab = Vide | N of 'a * 'a ab * 'a ab;;


type ('a,'b) abs = F of 'a | NI of 'b * ('a,'b) abs * ('a,'b) abs;;

let rec complet_ab n e =

	if n<=0 then Vide
	else N(e, complet_ab (n-1) e, complet_ab (n-1) e);;



let rec complet_abs n e =
	if n<=0 then F e
   else NI(e, complet_abs (n-1) e, complet_abs (n-1) e);;


let complet2_abs n =
	 let rec aux h k =
		if h<=0 then F k
		else NI(k, aux (h-1) (2*k), aux (h-1) (2*k+1))
	in aux n 1;;




(* ex 2*)


type 'a ag = NG of 'a * 'a ag list;;


let rec nb_feuilles = function
	|NG(_,[]) -> 1
	|NG(_,l) -> fold_left (fun acc t -> acc + nb_feuilles t) 0 l;;



let rec hauteur = function
	|NG(_,[]) -> 0
	|NG(_,l) -> 1 + fold_left (fun acc t -> max acc (hauteur t)) 0 l;;


(* ex 3*)

let rec max_ab = function
   |Vide -> failwith "vide"
	|N(v,g,d) ->
		 let mg = match g with Vide -> v | _ -> max_ab g in
		let md = match d with Vide -> v | _ -> max_ab d in
	     max v (max mg md);;



let rec max_abs = function
	|F v -> v
	|NI(v,g,d) -> max v (max (max_abs g) (max_abs d));;





let list_prof_ab a x =
	let rec aux t prof =
		 match t with
		|Vide -> []
		|N(v,g,d) ->
			 let l = if v=x then [prof] else [] in
			l @ (aux g (prof+1)) @ (aux d (prof+1))
in aux a 0;;



let list_prof_abs a x =
	let rec aux t prof =
		match t with
		|F v -> if v=x then [prof] else []
		|NI(v,g,d) ->
			let l = if v=x then [prof] else [] in
			l @ (aux g (prof+1)) @ (aux d (prof+1))
	in aux a 0;;


(* ex 4 *) 



let generation a n =
	let rec aux t prof =
		match t with
		|Vide -> []
		|N(v,g,d) ->
			if prof = n then [v]
			else (aux g (prof+1)) @ (aux d (prof+1))
	in aux a 0;;




let parcours_profondeur a =
	let rec aux = function
		|Vide -> ()
		|N(v,g,d) ->
			printf "%d\n" v;
			aux g;
			aux d
	in aux a;;





(* ex 5 *)
let somme_profondeurs a =
	let rec aux t prof =
		match t with
		|Vide -> 0
		|N(_,Vide,Vide) -> prof
		|N(_,g,d) -> (aux g (prof+1)) + (aux d (prof+1))
	in aux a 0;;

(*ex 6 *)

type strahler = F | N of strahler * strahler;;
