
(* fonction qui renvoie la case a droite de la case cible *)
let right (gen:generation) i j = if ((Array.length gen)-1)=j then gen.(i).(0) else gen.(i).(j+1);;


(* fonction qui renvoie la case en haut a droite de la case cible *)
let up_right (gen:generation) i j = if (i=0) && (((Array.length gen)-1)=j) then gen.((Array.length gen)-1).(0) 
				   else if i=0 then gen.((Array.length gen)-1).(j+1)
				   else if j=((Array.length gen)-1) then gen.(i-1).(0)
				   else gen.(i-1).(j+1);;


(* fonction qui renvoie la case en bas a droite de la case cible *)
let down_right (gen:generation) i j = if (i=(Array.length gen)-1) && ((Array.length gen)-1=j) then gen.(0).(0) 
				   else if i=((Array.length gen)-1) then gen.(0).(j+1)
				   else if j=((Array.length gen)-1) then gen.(i+1).(0)
				   else gen.(i+1).(j+1);;

(* fonction qui renvoie la case a gauche de la case cible *)
let left (gen:generation) i j = if j=0 then gen.(i).((Array.length gen)-1) else gen.(i).(j-1);;


(* fonction qui renvoie la case en haut a gauche de la case cible *)
let up_left (gen:generation) i j = if (i=0) && (0=j) then gen.((Array.length gen)-1).((Array.length gen) -1) 
				   else if i=0 then gen.((Array.length gen)-1).(j-1)
				   else if j=(0) then gen.(i-1).((Array.length gen)-1)
				   else gen.(i-1).(j-1);;


(* fonction qui renvoie la case en bas a gauche de la case cible *)
let down_left (gen:generation) i j = if (i=(Array.length gen)-1) && (0=j) then gen.(0).((Array.length gen)-1) 
				   else if i=((Array.length gen)-1) then gen.(0).(j-1)
				   else if j=(0) then gen.(i+1).((Array.length gen)-1)
				   else gen.(i+1).(j-1);;


(* fonction qui renvoie la case en bas de la case cible *)
let down (gen:generation) i j = if i=((Array.length gen)-1) then gen.(0).(j) else gen.(i+1).(j);;


(* fonction qui renvoie la case en haut de la case cible *)
let up (gen:generation) i j = if i=0 then gen.((Array.length gen)-1).(j) else gen.(i-1).(j);;

(* fonction aui renvoie un voisinage de von neumann  pour une cellule *)
let voisinage_von_neumann (gen:generation) i j = ((up gen i j),(right gen i j),(down gen i j),(left gen i j),(gen.(i).(j)));;

(* fonction auxiliere a la fonction next_generation elle vaq verifier si le voisinage donne en argument correspond a une des regles*)
let rec aux_next_gen (liste:automate) voisinage = match liste with 
	| [] -> false
	| a::q -> if a=voisinage then true else aux_next_gen q voisinage ;;

(* fonction qui va mettre a jour la generation donnee en argument avec les regles donne en argument *)
let next_generation ((automat:automate),(gen:generation)) = let aux ((automat:automate),(gen:generation)) tab =  
	for i=0 to (Array.length gen)-1 do 
		for j=0 to (Array.length gen)-1 do 
			if (aux_next_gen automat (voisinage_von_neumann gen i j)) then tab.(i).(j) <- A else tab.(i).(j) <- D
		done
	done ; (tab:generation) in  aux (automat,gen) (Array.make_matrix (Array.length gen) (Array.length gen) A);;

(* Fonction aui affiche une generation donnee en argument *)
let show_generation (gen:generation) = 
	for i=0 to ((Array.length gen)-1) do 
		for j=0 to ((Array.length gen)-1) do
			match (gen.(i).(j))with
				| A -> print_string "A "
				| D -> print_string "D "
		done ; print_string "\n"
	done ;;






(*
(* ********** Test des fonctions ********** *)
let (gen:generation) = Array.make_matrix 3 3 A;;
let rule = (A,A,A,A,A);;
let rule2 = (A,A,A,A,D);;
let rule3 = (A,A,A,D,A);;
let rule4 = (A,A,A,D,D);;
let automat = [rule;rule2;rule3;rule4];;
gen.(0).(0) <- D ;;
gen.(1).(1) <- D ;;
gen.(2).(2) <- D ;;
show_generation gen;;
let gen2 = next_generation (automat,gen) ;;
show_generation gen2;;
let gen3 = next_generation (automat,gen2) ;;
show_generation gen3;;

(********************************************)*)
