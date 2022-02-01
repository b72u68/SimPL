all: src/main.ml src/types.ml src/parser.mly src/lexer.mll src/dune
	dune build
	dune utop src

clean:
	dune clean
