open Types
open Parser
open Propositionnel
open Simuler
open Stable

let channel = open_in Sys.argv.(1) ;;
let couplet = parse channel ;; 

(* Fonction qui permet de voir une generation *)
let show (x,y,z) = show_generation z ;;
(* fonction qui permet d'avoir la generation suivante *)
let next (x,y,z) = next_generation (y,z) ;;
(* fonction qui verifie si le couple est correste *) 
let verifie_couplet (x,y,z) = if x=0 then false else true ;; 

if (verifie_couplet couplet ) = true then
print_string "Voici la génération initial \n";;

if (verifie_couplet couplet ) = true then
(show couplet);;

if (verifie_couplet couplet ) = true then
print_string "Voici la génération suivante \n";;

if (verifie_couplet couplet ) = true then
let ma_nouvelle = (next couplet) in 
(show_generation ma_nouvelle);;

print_string "****************************************** Recherche des générations stables ************************************\n";;
let fonction_stable (x,y,z) = stables (y,x);;
if (verifie_couplet couplet)=true then
(* fonction qui permet d'avoir une fnc pour trouver les generations stables *)
let formoule = fonction_stable couplet in 
let channel_out = open_out "entree.dimacs" in
creat_dimacs formoule channel_out ; 
show_stable()
else print_string " Nous ne pouvons pas générer les generations stables une erreur est survenue\n" ;;

