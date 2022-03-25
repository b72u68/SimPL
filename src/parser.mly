%{
    open Ast
%}

%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token AND OR
%token LT LE GT GE EQ NEQ EQUAL
%token LPAREN RPAREN
%token LBRACE RBRACE
%token IF ELSE
%token WHILE
%token QUESTION COLON SEMICOLON
%token SKIP
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start <Ast.stmt> prog

%%
prog:
    | stmts EOF      { Seq $1 }
;

iexpr:
    | INT                                               { Num $1 }
    | VAR                                               { Var $1 }
    | LPAREN bexpr QUESTION iexpr COLON iexpr RPAREN    { IfExpr ($2, $4, $6) }
    | iexpr PLUS iexpr                                  { Binop (Plus, $1, $3) }
    | iexpr MINUS iexpr                                 { Binop (Minus, $1, $3) }
    | iexpr TIMES iexpr                                 { Binop (Times, $1, $3) }
    | iexpr DIV iexpr                                   { Binop (Div, $1, $3) }
    | LPAREN iexpr RPAREN                               { $2 }
;

bexpr:
    | TRUE                              { True }
    | FALSE                             { False }
    | bexpr AND bexpr                   { And ($1, $3) }
    | bexpr OR bexpr                    { Or ($1, $3) }
    | iexpr LT iexpr                    { Relop (Lt, $1, $3) }
    | iexpr GT iexpr                    { Relop (Gt, $1, $3) }
    | iexpr LE iexpr                    { Relop (Le, $1, $3) }
    | iexpr GE iexpr                    { Relop (Ge, $1, $3) }
    | iexpr EQ iexpr                    { Relop (Eq, $1, $3) }
    | iexpr NEQ iexpr                   { Relop (Neq, $1, $3) }
    | LPAREN bexpr RPAREN               { $2 }
;

stmt:
    | VAR EQUAL iexpr SEMICOLON         { Assign ($1, $3) }
    | IF bexpr block ELSE block         { If ($2, $3, $5) }
    | WHILE bexpr block                 { While ($2, $3) }
    | SKIP SEMICOLON                    { Skip }
;

stmts:
    | stmt stmts    { $1::$2 }
    |               { [] }
;

block:
    | stmt                      { Seq [$1] }
    | LBRACE stmts RBRACE       { Seq $2 }
;
