(*#load "propositionnel.cma"*)
open Propositionnel;;
open Types;;
open Simuler;;
open Parser;;

let fic = open_out "entree.dimacs";;

let fnc = Et(Var("1"),Ou(Neg(Var("2")),Var("3")));;
let str f = string_of_formule f;;


(* transforme une fomule en chaine de charactere dans le format dimacs *)

let rec form_to_dimacs form = match form with
|Vrai -> failwith "presence d'une valuation vrai"
|Faux -> failwith "presence d'une valuation fause"
|Neg(f) -> "-"^(form_to_dimacs f)
|Var(v) -> v
|Et(f,g) -> (form_to_dimacs f)^" 0 \n" ^(form_to_dimacs g)^" 0\n" 
|Ou(f,g) -> (form_to_dimacs f)^" "^(form_to_dimacs g);;



(* calcule le nombre de clause dijonctive d'une formule *) 

let  nombre_de_clause form = 
let rec aux form = match form with
|Vrai|Faux -> 0
|Var(v)-> 0
|Neg(f)-> aux f
|Ou(f,g)-> (aux f)+(aux g)
|Et(f,g)-> (aux f)+(aux g)+1 in
(aux form)+1;;


(* calcule le nombre de variable propositionnel dans une formule *)

let nombre_de_var form = 
let rec aux form l = match form with
|Vrai|Faux -> l
|Var(v) -> if List.mem v l then l else v::l
|Et(f,g)-> aux g (aux f l)
|Ou(f,g)-> aux g (aux f l)
|Neg(f) -> aux f l in
List.length (aux form []);;


(* crée un fichier au format dimacs *)

let creat_dimacs form out_chan = 
output_string out_chan ("p cnf "^(string_of_int (nombre_de_var form))^" "^(string_of_int (nombre_de_clause form))^"\n"^(form_to_dimacs form));
close_out out_chan;;


(* transforme un string en liste de char *)

let explode str = 
let rec aux i l = 
if i<0 then l else aux (i-1) (str.[i]::l) in
let rec liste_f l l1 s= match l with
[] -> l1@[s]
| a::q-> if a = ' ' then liste_f q (l1@[s]) "" else liste_f q l1 (s^(String.make 1 a)) in 
liste_f (aux ((String.length str)-1) []) [] "" ;;



(* donne la generation obtenu avec la sortie de minisat sous forme de string *)
(* pas de 0 dans la sortie  de minisat || prendre la taille de la generation *)
let get_gen_stable str = 
let i = ref 0 in
let taille_liste = int_of_float(sqrt (float_of_int ((List.length (explode str))-1))) in
let rec aux l res = match l with
|[]|"0"::[]  -> res
|a::q ->i := !i+1; 
if (int_of_string a)<0 
then begin 
if (!i mod taille_liste)=0 
then aux q (res^"D\n") 
else aux q (res^"D")
end 
else
if (!i mod taille_liste)=0 
then aux q (res^"A\n")
else aux q (res^"A") 
in aux (explode str) "";; 


(* recupere un string du fichier de nom "str" *)

let get_string_in str = 
let fic = (open_in str) in
let rec get_line in_ch res=
try get_line in_ch (res^(input_line in_ch)) 
with End_of_file -> res
in get_line fic "";;

(* show stable *)

let show_stable () = 
let continue = ref true in 
let resultat = ref 0 in 
while (!continue && !resultat <> 3)
do begin
resultat := Sys.command "minisat entree.dimacs sortie";
if !resultat = 10 then begin
print_string "la formule est satisfaisable\n";
show_generation (strlist_to_statearray (get_list_from_file (get_gen_stable (get_string_in "sortie"))));
print_string "voulez vous continuer a trouver des generations stables?\n";
if read_line () = "non" then begin
continue := false 
end
end;
if !resultat = 20 then begin
print_string "la formule n'est pas satisfaisable";
print_newline() 
end;
end;
done;;

show_stable ();;

 
