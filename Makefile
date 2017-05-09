fun:
	ocamlbuild -use-ocamlfind -package qcheck fun.byte
	ocamlbuild -use-ocamlfind -package qcheck fun.cma

clean:
	ocamlbuild -clean
