CMO=lexer.cmo parser.cmo ppast.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=ppast

all: $(BIN)
	./$(BIN) 

$(BIN): $(CMO)
	ocamlc -o $(BIN) $(CMO)

#.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir --infer -v $<

parser.ml: ast.cmi

clean:
	rm *.cmi *.cmx *.cmo
	rm ppast
