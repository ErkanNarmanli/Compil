make:
	ocamlopt -o ppast ast.mli ppast.ml 

clean:
	rm *.cmi *.cmx *.o
	rm ppast
