all: lib/ast.ml lib/parser.mly lib/lexer.mll lib/eval.ml lib/dune bin/main.ml bin/dune
	dune build @fmt

utop: bin
	dune utop bin

clean: _build
	dune clean
