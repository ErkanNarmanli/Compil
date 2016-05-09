BIN=pscala

all:
	ocamlbuild src/$(BIN).native
	mv $(BIN).native $(BIN)

clean:
	ocamlbuild -clean
	rm -f $(BIN)

test: test1 test2 test3

test1: all
	./test -1 "./$(BIN) --parse-only"
test2: all
	./test -2 "./$(BIN) --type-only"
test3: all
	./test -3 ./$(BIN)
	rm -f out a.out
	rm -f tests/exec/*.s
	rm -f tests/exec-fail/*.s
	rm -f tests/exec_add/good/*.s

stats: all
	./stats -w

