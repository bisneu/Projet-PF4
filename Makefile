main: types.cmi parser.cmo simuler.cmo propositionnel.cmo graph.cmo stable.cmo main.cmo
	 ocamlc -o main parser.cmo simuler.cmo propositionnel.cmo unix.cma graphics.cma graph.cmo stable.cmo main.cmo
types.cmi: types.mli
	ocamlc  types.mli
parser.cmo: parser.ml types.cmi
	ocamlc -c parser.ml 
simuler.cmo: simuler.ml types.cmi
	ocamlc -c simuler.ml
propositionnel.cmo: propositionnel.ml types.cmi
	ocamlc -c propositionnel.ml
stable.cmo: stable.ml types.cmi
	ocamlc -c stable.ml
graph.cmo: graph.ml types.cmi 
	ocamlc -c graph.ml
main.cmo: main.ml types.cmi
	ocamlc -c main.ml
clean : 
	rm *.cmi *.cmo 
