#load "propositionnel.cma"
open Propositionnel;;
open Types;;

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

let creat_dimacs form out_chan = let out = open_out "entree.dimacs" in
 output_string out_chan ("p cnf "^(string_of_int (nombre_de_var form))^" "^(string_of_int (nombre_de_clause form))^"\n"^(form_to_dimacs form));
close_out out_chan;;


(* show stable *)
let show_stable () = 
let continue = true in 
while continue do Sys.command "minisat entree.dimacs";;;
