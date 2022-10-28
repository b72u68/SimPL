setup:
	opam install dune utop menhir

build: src/main.ml src/ast.ml src/parser.mly src/lexer.mll src/dune
	dune build

run: src
	dune utop src

clean: _build
	dune clean
