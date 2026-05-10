type 'a ab = Vide | Noeud of 'a * 'a ab * 'a ab;;
 
 
(* exo 1 *)
 
type arbre = F | N of arbre * arbre;;
 
let rec meme_structure a b = match a, b with
	|Vide, Vide -> true
	|Vide, _ | _, Vide -> false
	|Noeud(_,fg1,fd1), Noeud(_,fg2,fd2) -> meme_structure fg1 fg2 && meme_structure fd1 fd2;;
 
let rec sous_arbre a b = match b with
	|Vide -> a = Vide
	|Noeud(_,fg,fd) -> meme_structure a b || sous_arbre a fg || sous_arbre a fd;;
 
 
(* exo 2*)
 
let est_un_ABR arbre =
	let rec aux min max = function
		|Vide -> true
		|Noeud(a,fg,fd) -> a > min && a < max && aux min a fg && aux a max fd
	in aux min_int max_int arbre;;
 
let rec cherche x = function
	|Vide -> false
	|Noeud(a,fg,fd) ->
		if x = a then true
		else if x < a then cherche x fg
		else cherche x fd;;
 
let rec minimum = function
	|Vide -> failwith "arbre vide"
	|Noeud(a,Vide,_) -> a
	|Noeud(_,fg,_) -> minimum fg;;
 
let rec taille = function
	|Vide -> 0
	|Noeud(_,fg,fd) -> 1 + taille fg + taille fd;;
 
let rec keme k = function
	|Vide -> failwith "k trop grand"
	|Noeud(a,fg,fd) ->
		let t = taille fg in
		if k = t+1 then a
		else if k <= t then keme k fg
		else keme (k-t-1) fd;;
 
let rec ajout x = function
	|Vide -> Noeud(x,Vide,Vide)
	|Noeud(a,fg,fd) ->
		if x <= a then Noeud(a, ajout x fg, fd)
		else Noeud(a, fg, ajout x fd);;
 
let tri l =
	let abr = List.fold_right ajout l Vide in
	let rec infixe = function
		|Vide -> []
		|Noeud(a,fg,fd) -> infixe fg @ [a] @ infixe fd
	in infixe abr;;
 
let rec ajout_racine x = function
	|Vide -> Noeud(x,Vide,Vide)
	|Noeud(a,fg,fd) ->
		if x <= a then match ajout_racine x fg with
			|Noeud(y,g,d) -> Noeud(y, g, Noeud(a,d,fd))
			|Vide -> assert false
		else match ajout_racine x fd with
			|Noeud(y,g,d) -> Noeud(y, Noeud(a,fg,g), d)
			|Vide -> assert false;;
 
let rec extraire_max = function
	|Vide -> failwith "arbre vide"
	|Noeud(a,fg,Vide) -> (a, fg)
	|Noeud(a,fg,fd) -> let (m,fd') = extraire_max fd in (m, Noeud(a,fg,fd'));;
 
let supp_racine = function
	|Vide -> failwith "arbre vide"
	|Noeud(_,Vide,fd) -> fd
	|Noeud(_,fg,fd) -> let (m,fg') = extraire_max fg in Noeud(m,fg',fd);;
 
let rec supp x = function
	|Vide -> Vide
	|Noeud(a,fg,fd) ->
		if x = a then supp_racine (Noeud(a,fg,fd))
		else if x < a then Noeud(a, supp x fg, fd)
		else Noeud(a, fg, supp x fd);;
