CMO=ppast.cmo lexer.cmo parser.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=ppast

all: $(BIN)
	./$(BIN) 

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
	rm ppast
	rm $(GENERATED)

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli  > .depend

include .depend



