# SimPL: CS 536 Programming Language

A simple programming language designed for CS 536: Science of Programming course
at Illinois Tech.

## Overview

I created this parser for fun and to check whether given expressions in
the homework are valid or not, which could easily be done by hand in 15 minutes.
But why dot that when I could spend 24 hours to make a parser to automate it. It
would be fun! (It wasn't).

## Syntax

The syntax of SimPL language is described by the BNF grammar below:

```
op = + | - | * | / | > | < | >= | <= | = | && | ||

const = int | true | false

expr =
    | const
    | var
    | const list
    | var[expr]
    | (expr)
    | expr op expr
    | expr ? expr : expr
    | max(expr, expr)
    | min(expr, expr)
    | size(expr)

stmt =
    | x := expr
    | var[expr] := expr
    | if expr then { stmt } else { stmt }
    | while expr { stmt }
    | stmt; stmt
    | skip

value = const | const list
```

## Installation and Running

This project requires Ocaml package manager `opam`. Learn more about `opam` and
install it [here](https://opam.ocaml.org/).

```
# build executable for the parser
$ make

# parse given file
$ dune exec simpl <filename>

# open libaries in top level
$ make utop

# remove/clean build
$ make clean
```

## TODO

- [ ] Implement automated testing using `dune`

- [ ] Implement small-step and big-step evaluation

- [ ] Implement verification system

- [x] ~~Include position in error message~~

- [x] ~~Add array to the grammar~~

- [x] ~~Typecheck?~~
