type state = A | D ;;
type generation = (state array) array;;
type vonNeu1 =(state*state*state*state*state);;
type vonMoore = (state*state*state*state*state*state*state*state*state);;
type rule = R1 of ruleVonNeu1;; 
type automaton = ((rule list)*generation);;
let parse  = (1,2,3,4);;
let parse3 = (1,8,3,4);;
parse = parse3;;

