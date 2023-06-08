%{
    open Ast
%}

%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token AND OR
%token LT LE GT GE EQ NE
%token ASSIGN
%token MAX MIN SIZE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token IF ELSE
%token WHILE
%token QUESTION COLON SEMICOLON COMMA
%token SKIP
%token EOF

%left OR
%left AND
%left LT LE GT GE NE EQ
%left PLUS MINUS
%left TIMES DIV

%start <Ast.stmt> prog

%%
prog:
    | ss=stmt_list EOF      { ss }
;

const:
    | TRUE                                              { CBool true }
    | FALSE                                             { CBool false }
    | n=INT                                             { CInt n }
;

value:
    | c=const                                           { VConst c }
    | LBRACKET; cs=const_list; RBRACKET                 { VArr cs }
;

expr:
    | v=value                                                           { mk_exp (EVal v) $loc }
    | v=VAR                                                             { mk_exp (EVar v) $loc }
    | v=VAR; LBRACKET; e=expr; RBRACKET                                 { mk_exp (EArrIdx (v, e)) $loc }
    | LPAREN; e1=expr; QUESTION; e2=expr; COLON; e3=expr; RPAREN        { mk_exp (EIf (e1, e2, e3)) $loc }
    | MAX; LPAREN; e1=expr; COMMA; e2=expr; RPAREN                      { mk_exp (EFun (FMax, [e1; e2])) $loc }
    | MIN; LPAREN; e1=expr; COMMA; e2=expr; RPAREN                      { mk_exp (EFun (FMin, [e1; e2])) $loc }
    | SIZE; LPAREN; e=expr; RPAREN                                      { mk_exp (EFun (FSize, [e])) $loc }
    | LPAREN; e=expr; RPAREN                                            { e }
    | e=bexpr                                                           { e }
;

bexpr:
    | e1=expr; PLUS; e2=expr                            { mk_exp (EBinop (Plus, e1, e2)) $loc }
    | e1=expr; MINUS; e2=expr                           { mk_exp (EBinop (Minus, e1, e2)) $loc }
    | e1=expr; TIMES; e2=expr                           { mk_exp (EBinop (Times, e1, e2)) $loc }
    | e1=expr; DIV; e2=expr                             { mk_exp (EBinop (Div, e1, e2)) $loc }
    | e1=expr; AND; e2=expr                             { mk_exp (EBinop (And, e1, e2)) $loc }
    | e1=expr; OR; e2=expr                              { mk_exp (EBinop (Or, e1, e2)) $loc }
    | e1=expr; LE; e2=expr                              { mk_exp (EBinop (Le, e1, e2)) $loc }
    | e1=expr; LT; e2=expr                              { mk_exp (EBinop (Lt, e1, e2)) $loc }
    | e1=expr; GE; e2=expr                              { mk_exp (EBinop (Ge, e1, e2)) $loc }
    | e1=expr; GT; e2=expr                              { mk_exp (EBinop (Gt, e1, e2)) $loc }
    | e1=expr; NE; e2=expr                              { mk_exp (EBinop (Neq, e1, e2)) $loc }
    | e1=expr; EQ; e2=expr                              { mk_exp (EBinop (Eq, e1, e2)) $loc }
;

stmt:
    | v=VAR; ASSIGN; e=expr                                                         { mk_stmt (SAssign (mk_lhs (LHVar v) $loc, e)) $loc }
    | v=VAR; LBRACKET; e1=expr; RBRACKET; ASSIGN; e2=expr                           { mk_stmt (SAssign (mk_lhs (LHArr (v, e1)) $loc, e2)) $loc }
    | IF; e=expr; LBRACE; s1=stmt_list; RBRACE; ELSE; LBRACE; s2=stmt_list; RBRACE  { mk_stmt (SIf (e, s1, s2)) $loc }
    | WHILE; e=expr; LBRACE; s=stmt_list; RBRACE                                    { mk_stmt (SWhile (e, s)) $loc }
    | SKIP                                                                          { mk_stmt SSkip $loc }
;

stmt_list:
    ss=separated_nonempty_list(SEMICOLON, stmt)     { mk_stmt (SBlock ss) $loc }
;

const_list:
    cs=separated_nonempty_list(COMMA, const)        { cs }
;
