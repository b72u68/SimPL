all:
	opam install dune

build: lib/ast.ml lib/parser.mly lib/lexer.mll lib/dune bin/main.ml bin/dune
	dune build

utop: bin
	dune utop bin

clean: _build
	dune clean
