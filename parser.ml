open Types;;

(* identifie un caractere donnée à un état *)
let char_to_state c = match c with
	'a' -> (A:state)
	|'A' -> (A:state)
	|'d' -> (D:state)
	|'D' -> (D:state)
	|_ -> failwith "ni a ni d";;

(* analyse une chaine de caractere correspondant a une regle et stock les états dans une liste*)
let explode_to_state str = 
	let rec aux s l = match s with
	"" -> l
	|_ -> aux (String.sub s 1 ((String.length s)-1)) ((char_to_state (String.get s 0))::l) 
	in aux str [];;

(* convertie une chaine de caractere représentant une regle en un type régle *)
let str_to_rule str :rule= 
	if (String.length str) = 5 
	then 
	((char_to_state (String.get str 0)),
	(char_to_state (String.get str 1)),
	(char_to_state (String.get str 2)),
	(char_to_state (String.get str 3)),
	(char_to_state (String.get str 4)))
	else failwith "";;

(* renvoie une liste de regle *)
let liste_to_regles l : automate = 
	let rec aux ls lr = match ls with
	[] -> lr
	|a::q -> (str_to_rule a)::(aux q lr)
	in aux l [];;
 
(* fonction qui stock les regles donnée dans le fichier dans une liste *)
let  getregles l = 
	let rec aux l1 l2 = match l1 with
	[] -> failwith "fichier incomplet ou incompatible"
	|a::q -> if a <> "GenerationZero" then a::l2 else l2
	in aux l [];;  

(* fonction qui permet de generer l'automate *)
let rec getautomaton l = match l with
	[] -> failwith "pas de regles"
	|a::q -> if a = "regles" then liste_to_regles (getregles q) else getautomaton q;;

(* permet d'avoir un tableau correspondant a la génération donnée dans le fichier *)
let strlist_to_statearray l :generation= 
	let rec generation l1 l2= match l with 
	[] -> l2
	|a::q -> generation q ((Array.of_list (explode_to_state a))::l2)
	in (Array.of_list (generation l []));;

(* permet de generer la generation *)
let rec getgeneration l = match l with 
	[] -> failwith "pas de generation"
	|a::q -> if a = "generationzero" then strlist_to_statearray q else getgeneration q;;
 
(* permet d'avoir une liste de chaine de caracteres correspondant aux ligne écrite dans le fichier *)
let get_list_from_file in_ch = 
	let rec aux res = 
	let next = try Some (input_line in_ch)
	with End_of_file -> None
	in match next with
	None -> res 
	|Some(w) -> aux (w :: res)
	in aux [] ;;

(* fonction demandé dans le sujet =D !! *)
let parse in_ch =
	let rec aux l = match l with  
	[] -> failwith "fichier vide"
	|a :: q -> try ((int_of_string a), getautomaton l , getgeneration l) with 
	|Failure "fail" -> (0, ((A,A,A,A,A)::[]),Array.make_matrix 1 1 A)
	in aux (get_list_from_file in_ch);;

