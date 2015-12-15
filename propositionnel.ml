open Types;;


(* fonction qui utilise les lois de Morgan sur les négations*)
let rec descente_neg f = match f with 
        | Neg(g) -> begin match g with 
                        | Neg(g2) -> descente_neg g2 
                        | Et(g2,d2) -> Ou((Neg(descente_neg g2)), (Neg(descente_neg d2)))
                        | Ou(g2,d2) -> Et((Neg(descente_neg g2)),(Neg(descente_neg d2)))
                        | _ -> g 
                   end
        | Et(g,d) -> Et((descente_neg g) , (descente_neg d)) 
        | Ou(g,d) -> Ou((descente_neg g), (descente_neg d ))
        | _ -> f ;;

(* fonction qui distribue le connecteur ou *)
let rec descente_ou f = match f with
        | Ou(g, Et(g2,d2)) -> Et(Ou((descente_ou g),(descente_ou g2)),Ou((descente_ou g),(descente_ou d2))) 
        | Ou(Et(g2,d2),d) -> Et(Ou((descente_ou g2), (descente_ou d)),Ou((descente_ou d2),(descente_ou d)))
        | Et(g,d) -> Et((descente_ou g),(descente_ou d))
        | Ou(g,d) -> Ou((descente_ou g),(descente_ou d))
        | Neg(g) -> Neg((descente_ou g))
        | _ -> f;; 
(* fonction qui affiche la formule propositionnel en string *)
let rec string_of_formule f = match f with 
        | Et (g,d) -> "( "^ (string_of_formule g)^ " Et "^ (string_of_formule d)^ " ) "          
        | Ou (g,d) -> " ( "^ (string_of_formule g)^ " Ou "^ (string_of_formule d) ^" ) "          
        | Neg (g) -> " Neg( "^ (string_of_formule g) ^ " ) "
        | Vrai -> " Vrai "
        | Faux -> " Faux "
        | Var d ->  d;; 

(* qui transforme une formule particulière en fnc mais je ne me rappel plus la quelle XD *)
let fnc f = descente_ou ((descente_neg f));;


(* fonction qui genere toutes les regles qui se terminent par un A *)
let rec genere env liste =
	match env  with 
		| (A,A,A,A,A) -> liste
		| (x,y,z,t,A) -> if t=D then genere (x,y,z,A,A) ((x,y,z,A,A)::liste)  
				else if z=D then genere (x,y,A,D,A) ((x,y,A,D,A)::liste) 
				else if y=D  then genere (x,A,D,D,A) ((x,A,D,D,A)::liste)	
				else if x=D then genere (A,D,D,D,A) ((A,D,D,D,A)::liste)
				else genere (A,A,A,A,A) ((A,A,A,A,A)::liste);;


(*fonction qui supprime un element dans une liste sans doublons *)
let supprime_element_liste liste x = 
	let rec aux liste liste2 x = match liste with 
		| [] -> (List.rev liste2) 
		| a::q -> if a=x then ((List.rev liste2) @ q) else aux q (a::liste2) x in 
	aux liste [] x;; 


(* fonction qui retire les regles qui se termient par A qui se trouve dans un automate donnée *)
let rec les_bonnes_regles_A liste automaton =  match automaton with 
	| [] -> liste 
	| a::q -> les_bonnes_regles_A (supprime_element_liste liste a) q ;;

(* fonction qui garde les regles qui se termient par D qui se trouve dans un automate donnée *)
let les_bonnes_regles_D automaton = 
	let rec aux automaton liste = match automaton with
		|[]-> liste 
		| a::q -> match a with 
				| (_,_,_,_,D) -> aux q (a::liste) 
				| (_,_,_,_,_) -> aux q liste in 
	aux automaton [];;

(* fonction qui envoie le int qui correspond à la case de gauche *)
let get_ligne taille c = if c mod taille = 0 then c/taille else (c/taille)+1 ;; 

(* fonction qui generes les bonnes regles pour une FNC *)
let les_bonnes_regles automaton = (les_bonnes_regles_A (genere (D,D,D,D,A) ((D,D,D,D,A)::[])) automaton )@(les_bonnes_regles_D automaton);;
			 
(* fonction qui envoie le int qui correspond à la case du dessu *)
let get_up_number case taille = if (case-taille)<0 then (taille*taille)+(case-taille) else (case-taille) ;; 
(* fonction qui envoi le int qui correspond à la case du bas *)
let get_down_number case taille = if (case+taille)>(taille*taille) then (case+taille) mod taille else (case+taille);;
(* fonction qui envoie le int qui correspond à la case de gauche *)
let get_left_number case taille ligne = if (case-1)<=0 then (ligne*taille) else (case-1);;
(* fonction qui envoie le int qui correspond à la case de droite *)
let get_right_number case taille ligne = if (get_ligne taille (case+1)) = ligne then (case+1) else case-(taille-1);;	

(* fonction qui renvoie le quituplet de case représenté par un string qui correspond à l'environnement de la case donnée en argument*)
let get_environement_case case taille = 
(string_of_int (get_up_number case taille),string_of_int (get_left_number case taille (get_ligne taille case)),string_of_int (get_down_number case taille),string_of_int (get_right_number case taille (get_ligne taille case)),string_of_int case);;

(* fonction qui renvoie une variable ou bien son nié  *)
let neg_or_pos case valeur = if valeur=D then Var case else Neg(Var case);;

(* fonction qui renvoie un clause pour un environement donné *)
let env_to_clause (a,b,c,d,e) (x,y,z,t,v) = Ou((neg_or_pos a x),Ou((neg_or_pos b y),Ou((neg_or_pos c z),Ou((neg_or_pos d t),(neg_or_pos e v)))));;


let fnc_partielle case regles taille = 
	let rec aux env regles liste = match regles with 
		| []->liste 
		| a::q->aux env q ((env_to_clause env a)::liste) in 
	aux (get_environement_case case taille) regles [];;

let sous_stables ((automaton:automate),i) = 
	let rec aux regles cmp taille liste = 
		if cmp = 1 then liste else aux regles (cmp-1) taille (fnc_partielle cmp regles taille)@liste in 
	aux (les_bonnes_regles automaton) i i [] ;; 

let et_formule a b = Et(a,b);;

let stables q = match sous_stables q with 
	| [] -> Faux 
	| a::q -> List.fold_left et_formule a q;;

let aut = [(D, A, D, A, D); (D, A, D, D, A); (D, A, A, A, D); (D, A, A, D, A);
      (D, D, D, A, A); (D, D, D, D, D); (D, D, A, A, A); (D, D, A, D, D);
      (A, A, D, A, D); (A, A, D, D, A); (A, A, A, A, A); (A, A, A, D, A);
      (A, D, D, A, A); (A, D, D, D, D)];;

string_of_formule (stables (aut,5));;
