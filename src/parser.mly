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
%token COMMA
%token MAX MIN
%token IF THEN ELSE
%token EOF

%start expr
%type <Types.exp> expr

%%
expr:
    | INT                           { Int $1 }
    | TRUE                          { Bool true }
    | FALSE                         { Bool false }
    | VAR                           { Var $1 }
    | expr op expr                  { Op ($2, $1, $3) }
    | IF expr THEN expr ELSE expr   { If ($2, $4, $6) }
    | LPAREN expr RPAREN            { $2 }
    | MAX LPAREN expr COMMA expr RPAREN { Max ($3, $5) }
    | MIN LPAREN expr COMMA expr RPAREN { Min ($3, $5) }
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
