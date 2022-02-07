%{
    open Ast
%}

%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token AND OR
%token LT LE GT GE EQ EQUAL
%token LPAREN RPAREN
%token SQLBRACKET SQRBRACKET
%token LBRACKET RBRACKET
%token MAX MIN
%token NTH
%token SIZE
%token IF THEN ELSE
%token WHILE
%token QUESTION COLON COMMA SEMICOLON
%token SKIP
%token EOF

%start <Ast.stmt> prog

%%
prog:
    | stmt EOF      { $1 }
;

expr:
    | const                             { Const $1 }
    | VAR                               { Var $1 }
    | expr op expr                      { Op ($2, $1, $3) }
    | expr QUESTION expr COLON expr     { IfExp ($1, $3, $5) }
    | LPAREN expr RPAREN                { $2 }
    | MAX LPAREN expr COMMA expr RPAREN { Op (Max, $3, $5) }
    | MIN LPAREN expr COMMA expr RPAREN { Op (Min, $3, $5) }
    | VAR SQLBRACKET expr SQRBRACKET    { Nth ($1, $3) }
    | SIZE LPAREN lst RPAREN            { Size  $3 }
;

stmt:
    | VAR EQUAL expr                                { Let ($1, $3) }
    | VAR SQLBRACKET expr SQRBRACKET EQUAL expr     { LetNth ($1, $3, $6) }
    | IF expr THEN stmt ELSE stmt                   { If ($2, $4, $6) }
    | WHILE expr LBRACKET stmt RBRACKET             { While ($2, $4) }
    | stmt SEMICOLON stmt                           { Seq ($1, $3) }
    | SKIP                                          { Skip }
;

op:
    | PLUS      { Plus }
    | MINUS     { Minus }
    | TIMES     { Times }
    | DIV       { Div }
    | AND       { And }
    | OR        { Or }
    | LT        { Lt }
    | LE        { Le }
    | GT        { Gt }
    | GE        { Ge }
    | EQ        { Eq }
;

const:
    | INT       { Int $1 }
    | TRUE      { True }
    | FALSE     { False }
;

lst:
    | SQLBRACKET separated_list(COMMA, const) SQRBRACKET    { $2 }
;
