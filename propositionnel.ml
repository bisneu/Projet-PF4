open Types;;

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


let rec descente_ou f = match f with
        | Ou(g, Et(g2,d2)) -> Et(Ou((descente_ou g),(descente_ou g2)),Ou((descente_ou g),(descente_ou d2))) 
        | Ou(Et(g2,d2),d) -> Et(Ou((descente_ou g2), (descente_ou d)),Ou((descente_ou d2),(descente_ou d)))
        | Et(g,d) -> Et((descente_ou g),(descente_ou d))
        | Ou(g,d) -> Ou((descente_ou g),(descente_ou d))
        | Neg(g) -> Neg((descente_ou g))
        | _ -> f;; 

let rec string_of_formule f = match f with 
        | Et (g,d) -> "( "^ (string_of_formule g)^ " Et "^ (string_of_formule d)^ " ) "          
        | Ou (g,d) -> " ( "^ (string_of_formule g)^ " Ou "^ (string_of_formule d) ^" ) "          
        | Neg (g) -> " Neg( "^ (string_of_formule g) ^ " ) "
        | Vrai -> " Vrai "
        | Faux -> " Faux "
        | Var d ->  d;; 

let fnc f = descente_ou ((descente_neg f));;  

