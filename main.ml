open Types
open Parser
open Propositionnel
open Simuler
open Stable

(* int*automaton*generation *)
let channel = open_in "monzizi.txt";;
let couplet = parse channel;;
print_string "\n";;

let show (x,y,z) = show_generation z ;;
let next (x,y,z) = next_generation (y,z) ;;
show couplet;;
let ma_nouvelle = next couplet;;
print_string "\n";;
show_generation ma_nouvelle;;
let fonction_stable (x,y,z) = stables (y,x) ;;
let formoule = fonction_stable couplet;;
let channel_out = open_out "entree.dimacs";;
creat_dimacs formoule channel_out ;; 
show_stable();

