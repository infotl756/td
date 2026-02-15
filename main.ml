let expo_rapide_terminale x n = 
    let rec aux acc x n =
        if n = 0 then acc else match n mod 2 with
            | 0 -> aux acc (x*x) (n/2)
            | _ -> aux (acc*x) (x*x) ((n-1)/2)
    in aux 1 x n;;

let c1 = expo_rapide_terminale 5 3;;

let syracuse n p = 
    let rec aux acc n =
        if n = 0 then acc else match acc mod 2 with
            | 0 -> aux (acc/2) (n-1)
            | _ -> aux (3*acc+1) (n-1)
    in aux p n;;

let c2 = syracuse 9 87;;

let affiche n p =
    let n0 = n in    
        let rec aux acc n =
                
                print_string "Le terme u_";
                print_int (n0-n) ;
                print_string " = ";
                print_int (acc);
                print_newline ();
                
                if n = 0 then acc else match acc mod 2 with
                    | 0 -> aux (acc/2) (n-1)
                    | _ -> aux (3*acc+1) (n-1)
            in aux p n;;

let c3 = affiche 9 87;;

let tempsdeVol p =
    let rec aux acc n =
        if acc = 1 then n else match acc mod 2 with
            | 0 -> aux (acc/2) (n+1)
            | _ -> aux (3*acc+1) (n+1)
    in aux p 0;;

let c4 = tempsdeVol 37;;

let hauteur p =
    let rec aux acc n max_t =
        if acc = 1 then max_t else match acc mod 2 with
            | 0 -> aux (acc/2) (n+1) (max acc max_t)
            | _ -> aux (3*acc+1) (n+1) (max acc max_t)
    in aux p 0 p;;

let c5 = hauteur 2049;;

let somme p = 
    let rec aux acc n s =
        if acc = 1 then (s+1) else match acc mod 2 with
            | 0 -> aux (acc/2) (n+1) (s+acc)
            | _ -> aux (3*acc+1) (n+1) (s+acc)
    in aux p 0 0;;

let c6 = somme 2049;;