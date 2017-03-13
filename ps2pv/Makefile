.PHONY: all clean dep
all:
	ocamlbuild -j 4 -use-ocamlfind jsdefp.native

web:
	ocamlbuild -use-ocamlfind -package js_of_ocaml jsdefp.byte
	js_of_ocaml -o jsdefp.js jsdefp.byte

clean:
	ocamlbuild -clean

dep:
	ocamlfind -query pcre str unix menhirLib ulex camlp4 -format "%p (%D): %v"

