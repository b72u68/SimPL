setup:
	opam install dune utop menhir

build: lib/ast.ml lib/parser.mly lib/lexer.mll lib/dune bin/simpl.ml bin/dune
	dune build

utop: bin
	dune utop bin

clean: _build
	dune clean
