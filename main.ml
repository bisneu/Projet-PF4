open Types
open Parser
open Propositionnel
open Simuler
open Stable
open Graph

let couplet = try ref (parse (open_in Sys.argv.(1))) with
|Sys_error x -> print_string "vous n'avez pas donné d'argument a main \n Usage: ./main [file]"; ref (0,[(A,A,A,A,A)],Array.make_matrix 1 1 A)
|_ ->print_string "Exception";ref (0,[(A,A,A,A,A)],Array.make_matrix 1 1 A);;

(*let channel = open_in Sys.argv.(1) ;;
let couplet = parse channel ;; 
*)
(* Fonction qui permet de voir une generation *)
let show (x,y,z) = init z;; (*show_generation z ;;*)
(* fonction qui permet d'avoir la generation suivante *)
let next (x,y,z) = next_generation (y,z) ;;
(* fonction qui verifie si le couple est correste *) 
let verifie_couplet (x,(y:automate),(z:generation)) = if x<0 then false else true ;; 

let run (x,y,z) =
let nextgen = ref (x,y,z) in
while true do
show !nextgen;
Unix.sleep 5;
nextgen :=(x,y, (next !nextgen));
done;;

let rec menu () = if (Array.length Sys.argv)<2 then 
begin
print_string "Usage: ./main [file] \n";
exit 1;
end;
print_string ("vous souhaitez: \n 1.lancer l'automate celulaire de "^Sys.argv.(1)^"\n 2.trouver une generation stable pour la liste de règles donnée dans "^Sys.argv.(1)^"\n choisissez 1, 2 ou exit pour quitter\n");
match read_line () with
|"1"-> run !couplet
|"2"-> show_stable ()
|"exit"-> exit 0
|_ -> print_string "veuillez taper 1, 2 ou exit pour quitter"; menu ();; 

(*
if verifie_couplet couplet then
print_string "Voici la génération initial \n";;

if verifie_couplet couplet then 
show couplet;;

if (verifie_couplet couplet ) = true then
print_string "Voici la génération suivante \n";;

if (verifie_couplet couplet ) = true then
let ma_nouvelle = (next couplet) in 
(show_generation ma_nouvelle);;

print_string "****************************************** Recherche des générations stables ************************************\n";;*)
let fonction_stable (x,y,z) = stables (y,x);;
(*if verifie_couplet !couplet then
(* fonction qui permet d'avoir une fnc pour trouver les generations stables *)
let formoule = fonction_stable !couplet in 
let channel_out = open_out "entree.dimacs" in
creat_dimacs formoule channel_out ; 
show_stable()
else print_string " Nous ne pouvons pas générer les generations stables une erreur est survenue\n" ;;
*)
menu ();;
