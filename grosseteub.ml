(*parce que ma teub est plus grosse *)

let fichier = open_in Sys.argv(1);;

type state = A | D ;;
type generation = (state array) array;;
(*type vonNeu1 =(state*state*state*state*state);;*)
(*type vonMoore = (state*state*state*state*state*state*state*state*state);;*)
type rule = (state*state*state*state*state);; 
type automaton = rule list;;

let char_to_state c = match c with
'a' -> (A:state)
|'A' -> (A:state)
|'d' -> (D:state)
|'D' -> (D:state)
|_ -> failwith "ni a ni d";;

let str_to_rule str :rule= 
if (String.length str) = 5 
then 
((char_to_state (String.get str 0)),
(char_to_state (String.get str 1)),
(char_to_state (String.get str 2)),
(char_to_state (String.get str 3)),
(char_to_state (String.get str 4)))
else failwith "";;

let liste_to_regles l : automaton = 
let rec aux ls lr = match ls with
[] -> lr
|a::q -> (str_to_rule a)::(aux q lr)
in aux l [];;
 
let  getregles l = 
let rec aux l1 l2 = match l1 with
[] -> failwith "fichier incomplet ou incompatible"
|a::q -> if a <> "GenerationZero" then a::l2 else l2
in aux l [];;  

let getgeneration ls = ls;;

let get_list_from_file in_ch = 
let rec aux res = 
let next = try Some (input_line in_ch)
with End_of_file -> None
in match next with
None -> res 
|Some(w) -> aux (w :: res)
in aux [] ;;

let parse in_ch = 
let aux ls = (gettaille ls,
