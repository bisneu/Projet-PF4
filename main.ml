open Types
open Parser
open Propositionnel
open Simuler
open Stable
open Graph

(* Reference vers le triplet obtenu à partir du fichier donné en argument *) 
let couplet = try ref (parse (open_in Sys.argv.(1))) with
  |Sys_error x -> print_string "vous n'avez pas donné d'argument a main \n Usage: ./main [file]"; ref (0,[(A,A,A,A,A)],Array.make_matrix 1 1 A)
  |_ ->print_string "Exception: ";ref (0,[(A,A,A,A,A)],Array.make_matrix 1 1 A);;

(* Fonction qui permet de voir une generation *)
let show (x,y,z) = init z;;

(* fonction qui permet d'avoir la generation suivante *)
let next (x,y,z) = next_generation (y,z) ;;

(* fonction qui verifie si le couple est correste *) 
let verifie_couplet (x,(y:automate),(z:generation)) = if x<=0 then false else true ;; 

(* fonction qui lance l'automate cellulaire *)
let run (x,y,z) =
  let nextgen = ref (x,y,z) in
    while true do
      show !nextgen;
    Unix.sleep 3;
    nextgen :=(x,y, (next !nextgen));
    done;;


(* menu qui apparait lors du demmarage du programme *)
let rec menu () = Sys.command "clear";
  if (Array.length Sys.argv)<2 then 
    begin
    print_string "Usage: ./main [file] \n";
    exit 1;
    end;
  if not (verifie_couplet !couplet) then 
    begin 
    print_string "Le fichier n'est pas conforme\n";
    exit 1;
    end;
  print_string ("########################## Automate Cellulaire ##########################\n
 vous souhaitez: \n 1.lancer l'automate celulaire de "^Sys.argv.(1)^" \n 2.trouver une generation stable pour la liste de règles donnée dans "^Sys.argv.(1)^"\n choisissez 1, 2 ou exit pour quitter\n");
  match read_line () with
  |"1"-> run !couplet
  |"2"-> show_stable ()
  |"exit"-> exit 0
  |_ -> print_string "veuillez taper 1, 2 ou exit pour quitter"; menu ();; 

let fonction_stable (x,y,z) = stables (y,x);;

(* demmarage du programme *)
menu ();;
