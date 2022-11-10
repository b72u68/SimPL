# SimPL: CS 536 Programming Language

A simple programming language designed for CS 536: Science of Programming course
at Illinois Tech.

## Overview

I created this parser for fun and to check whether the given expressions in
the homework are valid or not, which can easily be done by hand in 15 minutes,
but I had decided that why not spend 24 hours to make a parser to automate it.
It would be fun (it wasn't).

The BNF for SimPL is as followed:

```
op = + | - | * | / | > | < | >= | <= | == | && | ||

const = int | true | false

arr = const list

expr =
    | const
    | var
    | (expr)
    | expr op expr
    | expr ? expr : expr
    | max(expr, expr)
    | min(expr, expr)
    | arr[expr]
    | size(arr)

stmt =
    | x = expr
    | arr[expr] = const list
    | arr[expr] = expr
    | if expr then { stmt } else { stmt }
    | while expr { stmt }
    | stmt; stmt
    | skip
```

## Installation and Running

This project requires Ocaml package manager `opam`. Learn more about `opam` and
install it [here](https://opam.ocaml.org/).

```
# install dependencies
$ make

# build parser executable
$ make build

# run the parser
$ ./simpl <filename>

# open project in top level
$ make utop

# remove/clean build
$ make clean
```

## TODO

- [ ] Typecheck?

- [ ] Implement a function to perform small-step and big-step operational semantics

- [ ] Implement verification system

- [x] ~~Add array to the grammar~~
