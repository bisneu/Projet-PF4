ocamlc -c types.ml
ocamlc -c parser.ml
ocamlc -c simuler.ml
ocamlc -c propositionnel.ml
ocamlc -c stable.ml
ocamlc -c main.ml

ocamlc -o toto types.cmo parser.cmo simuler.cmo propositionnel.cmo stable.cmo main.cmo 
#ocamlc -a types.ml -o types.cma
#ocamlc -a parser.ml -o parser.cma
#ocamlc -a simuler.ml -o simuler.cma
#ocamlc -a propositionnel.ml -o propositionnel.cma
#ocamlc -a stable.ml -o stable.cma
