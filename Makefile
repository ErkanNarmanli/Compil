CMO=ppast.cmo lexer.cmo parser.cmo tast.cmo misc.cmo context.cmo substitution.cmo printing.cmo variance.cmo typer.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli parser.automaton
BIN=main
FLAGS=-w +A-4

make: $(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc $(FLAGS) -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir --infer -v $<

parser.ml: ast.cmi

clean:
	rm *.cmi *.cmo
	rm main
	rm $(GENERATED)

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli  > .depend

include .depend

test: make
	./test -2 ./$(BIN)

stats: make
	./stats -w


