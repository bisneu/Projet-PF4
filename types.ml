type state = A | D ;;
type generation = (state array) array;;
(*type vonNeu1 =(state*state*state*state*state);;*)
(*type vonMoore = (state*state*state*state*state*state*state*state*state);;*)
type rule = (state*state*state*state*state);; 
type automaton = rule list;;
