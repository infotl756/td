let diviseur a b = match (a mod b) with 
 | 0 -> true
 | _ -> false;;

let maximum a b = match (a > b) with 
 | true -> a
 | _ -> b;;

let compose f g x = f (g x);;

let carre x = x*x;;
let affine x = 3*x;;

let c = compose carre affine 3;;

type complexe = {re:float; im:float};;
let z = {re=3.;im=6.}
let im_z = z.im

type complexe2 = Complexe of float * float;;
let z2 = Complexe (4., 5.9);;


let affiche_complexe z = 
  print_float z.re;
  print_string " + i";
  print_float z.im;;
let c = affiche_complexe z;;

exception Complexe_nul;;

(*
let inverse z = try 1 /. z.re*.z.re + z.im*.z.im with 
 | Complexe_nul -> failwith "Complexe nul"
 | _ -> {re=z.re /. (z.re*z.re + z.im*z.im) ; im= -. z.im /. (/. z.re*z.re + z.im*z.im)};;
*)

let inverse2 z = match z.re*.z.re + z.im*.z.im with
 | 0. -> raise Complexe_nul
 | _ -> {re=z.re/.(z.re*.z.re + z.im*.z.im), im=-.z.im/(z.re*.z.re + z.im*.z.im)};;
(*pas calculer d 3 fois*)

let argument z = match z.re*.z.re + z.im*.z.im with
 | 0. -> raise Complexe_nul
 | _ -> 
  