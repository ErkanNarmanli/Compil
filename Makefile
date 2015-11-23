CMO=ppast.cmo lexer.cmo parser.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=main

make: $(BIN)
	for f in *.scala; do ./$(BIN) $$f; done 

$(BIN): $(CMO)
	ocamlc -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir --infer -v $<

parser.ml: ast.cmi
ppast.ml: ast.cmi

clean:
	rm *.cmi *.cmo
	rm main
	rm $(GENERATED)

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli  > .depend

include .depend


