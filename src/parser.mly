%{
    open Types
%}

%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token AND OR
%token LT LE GT GE EQ
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token COMMA
%token MAX MIN
%token NTH
%token SIZE
%token IF THEN ELSE
%token EOF

%start <Types.exp> prog

%%
prog:
    | expr EOF      { $1 }
;

expr:
    | const                         { Const $1 }
    | VAR                           { Var $1 }
    | expr op expr                  { Op ($2, $1, $3) }
    | IF expr THEN expr ELSE expr   { If ($2, $4, $6) }
    | LPAREN expr RPAREN            { $2 }
    | MAX LPAREN expr COMMA expr RPAREN { Max ($3, $5) }
    | MIN LPAREN expr COMMA expr RPAREN { Min ($3, $5) }
    | NTH LPAREN lst COMMA expr RPAREN  { Nth ($3, $5) }
    | lst LBRACKET expr RBRACKET        { Nth ($1, $3) }
    | SIZE LPAREN lst RPAREN            { Size  $3 }
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
    | LBRACKET; separated_list(COMMA, const); RBRACKET    { List $2 }
;
