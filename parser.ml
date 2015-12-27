open Types;;

exception FileError ;;
exception RulesError ;;
exception EmptyError ;;
exception SyntaxeError ;;
exception StateError ;;
exception GenerationError ;; 

(* identifie un caractere donnée à un état *)
let char_to_state c = match c with
	|'a' -> (A:state)
	|'A' -> (A:state)
	|'d' -> (D:state)
	|'D' -> (D:state)
	|_ -> raise StateError;;

(* analyse une chaine de caractere correspondant a une regle et stock les états dans une liste*)
let explode_to_state str = 
	let rec aux s l = match s with
	"" -> l
	|_ -> aux (String.sub s 1 ((String.length s)-1)) (l@[(char_to_state (String.get s 0))]) 
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
	|a::q -> (aux q ((str_to_rule a )::lr))
	in aux l [];;
 
(* fonction qui stock les regles donnée dans le fichier dans une liste *)
let  getregles l = 
	let rec aux l1 l2 = match l1 with
	[] -> raise SyntaxeError 
	|a::q -> if a <> "GenerationZero" then aux q (a::l2) else l2
	in aux l [];;  

(* fonction qui permet de generer l'automate *)
let rec getautomaton l = match l with
	[] -> raise RulesError
	|a::q ->  begin match a with
	| "regles"-> liste_to_regles (getregles q) 
	|_ -> getautomaton q end;;

(* permet d'avoir un tableau correspondant a la génération donnée dans le fichier *)
let strlist_to_statearray l :generation= 
	let rec generation l1 l2 = match l1 with 
	[] -> l2
	|""::[]-> raise SyntaxeError
	|a::q -> generation q (l2@[(Array.of_list (explode_to_state a))])
	in (Array.of_list (generation l []));;

(* permet de generer la generation *)
let rec getgeneration l = match l with 
	[] -> raise GenerationError 
	|a::q ->  if a = "GenerationZero" then strlist_to_statearray q else getgeneration q;;
 
(* permet d'avoir une liste de chaine de caracteres correspondant aux ligne écrite dans le fichier *)
let get_list_from_file in_ch = 
	let rec aux res = 
	let next = try Some (input_line in_ch)
	with End_of_file -> None
	in match next with
	None -> res 
	|Some(w) ->  aux (res@[w]) 
	in aux [] ;;

(* fonction demandé dans le sujet =D !! *)
let parse in_ch =
	let aux l = match l with  
	[] -> raise EmptyError  
	|a :: q -> try ((int_of_string a),(getautomaton q),(getgeneration q)) with 
	|Failure "La saisie est mauvaise" -> (0, ((A,A,A,A,A)::[]),Array.make_matrix 1 1 A)
	in  try aux (get_list_from_file in_ch)
	    with | FileError -> print_string "Il y a un problème de fichier veuillez verifier la syntaxe \n" ; (0,((A,A,A,A,A)::[]),Array.make_matrix 1 1 A ) 
	     	 | RulesError -> print_string "Problème dans les règles veuillez verifier la syntaxe des règles" ; (0,((A,A,A,A,A)::[]),Array.make_matrix 1 1 A ) 
	     	 | EmptyError -> print_string "Le fichier est vide \n" ; (0,((A,A,A,A,A)::[]),Array.make_matrix 1 1 A ) 
	     	 | SyntaxeError -> print_string "Verifier la syntaxe du fichier\n" ; (0,((A,A,A,A,A)::[]),Array.make_matrix 1 1 A ) 
	     	 | StateError -> print_string "Il y a un probleme dans les states utilisez seulement A ou D\n" ; (0,((A,A,A,A,A)::[]),Array.make_matrix 1 1 A ) 
	     	 | GenerationError -> print_string "Problème dans la génération \n" ; (0,((A,A,A,A,A)::[]),Array.make_matrix 1 1 A );; 

