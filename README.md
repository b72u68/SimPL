# CS 536 Programming Language

A parser for a simple programming language designed for CS 536: Science of
Programming course at Illinois Tech.

## Overview

I created this parser for fun and to check whether the given expressions in
the homework are legal or not, which can easily be done by hand in 15 minutes but
I had decided that why not spend 24 hours to make a parser to automate it. It
would be fun.

The grammar for this progamming language is as followed:

```
op = "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<=" | "==" | "&&" | "||"
const = int | true | false
lst = const list
expr =
    | const
    | var
    | (expr)
    | expr op expr
    | expr ? expr : expr
    | max(expr, expr) | min(expr, expr)
    | arr[expr]
    | size(arr)
```

## Installation and Running

This project requires Ocaml package manager `opam`.

```
# install dependencies
$ opam install dune utop menhir

# build parser (this will run utop)
$ make

# remove/clean build
$ make clean
```
