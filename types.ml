(*type pour l'automate *)
type state = A | D ;;
type generation = (state array) array;;
(*type vonNeu1 =(state*state*state*state*state);;*)
(*type vonMoore = (state*state*state*state*state*state*state*state*state);;*)
type rule = (state*state*state*state*state);; 
type automate = rule list;;
(* type pour les formules propositionnelles*)
type formule = Vrai | Faux
              | Var of string
              | Neg of formule
              | Et of formule * formule 
              | Ou of formule * formule;;

exception FileError;;  
exception RulesError;;  
exception SizeError;;  
exception SyntaxeError;;  
