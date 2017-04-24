byte:
	ocamlbuild -use-ocamlfind -yaccflag -v src/main.byte

top:
	ocamlbuild -use-ocamlfind src/main.top

js:
	ocamlbuild -use-menhir -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o src/jsbridge.byte
	js_of_ocaml --pretty --noinline jsbridge.byte

test:
	ocamlbuild -use-ocamlfind -package oUnit src/test.byte

domcheck:
	ocamlbuild -use-ocamlfind -package qcheck src/redomcheck.byte

domcheck.top:
	ocamlbuild -use-ocamlfind -package qcheck src/redomcheck.top

clean:
	ocamlbuild -clean
	rm -f *~ src/*~ examples/*~
