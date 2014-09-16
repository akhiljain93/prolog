all: Interpreter.ml PrologParser.mly PrologScanner.mll types.ml
	ocamlc -c types.ml
	ocamlyacc PrologParser.mly
	ocamlc -c PrologParser.mli
	ocamllex PrologScanner.mll
	ocamlc -c PrologScanner.ml
	ocamlc -c PrologParser.ml
	ocamlc -c Interpreter.ml
	ocamlc -o calc PrologScanner.cmo PrologParser.cmo Interpreter.cmo
	rm -f PrologScanner.ml *.cmo *.cmi PrologParser.ml PrologParser.mli 2> /dev/null
clean:
	rm -f PrologScanner.ml *.cmo *.cmi PrologParser.ml PrologParser.mli calc 2> /dev/null