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
    | ss=stmt_list EOF      { SBlock ss }
;

const:
    | TRUE                                              { EBool true }
    | FALSE                                             { EBool false }
    | n=INT                                             { EInt n }
;

expr:
    | c=const                                           { EConst c }
    | v=VAR                                             { EVar v }
    | LBRACKET; cs=const_list; RBRACKET                 { EArr cs }
    | v=VAR; LBRACKET; e=expr; RBRACKET                 { EArrIdx (v, e) }
    | LPAREN; e1=expr; QUESTION; e2=expr; COLON; e3=expr; RPAREN        { EIf (e1, e2, e3) }
    | MAX; LPAREN; e1=expr; COMMA; e2=expr; RPAREN      { EFun (FMax, [e1; e2]) }
    | MIN; LPAREN; e1=expr; COMMA; e2=expr; RPAREN      { EFun (FMin, [e1; e2]) }
    | SIZE; LPAREN; e=expr; RPAREN                      { EFun (FSize, [e]) }
    | LPAREN; e=expr; RPAREN                            { e }
    | e=bexpr                                           { e }
;

bexpr:
    | e1=expr; PLUS; e2=expr                            { EBinop (Plus, e1, e2) }
    | e1=expr; MINUS; e2=expr                           { EBinop (Minus, e1, e2) }
    | e1=expr; TIMES; e2=expr                           { EBinop (Times, e1, e2) }
    | e1=expr; DIV; e2=expr                             { EBinop (Div, e1, e2) }
    | e1=expr; AND; e2=expr                             { EBinop (And, e1, e2) }
    | e1=expr; OR; e2=expr                              { EBinop (Or, e1, e2) }
    | e1=expr; LE; e2=expr                              { EBinop (Le, e1, e2) }
    | e1=expr; LT; e2=expr                              { EBinop (Lt, e1, e2) }
    | e1=expr; GE; e2=expr                              { EBinop (Ge, e1, e2) }
    | e1=expr; GT; e2=expr                              { EBinop (Gt, e1, e2) }
    | e1=expr; NE; e2=expr                              { EBinop (Neq, e1, e2) }
    | e1=expr; EQ; e2=expr                              { EBinop (Eq, e1, e2) }
;

stmt:
    | v=VAR; ASSIGN; e=expr                                                         { SAssign (LHVar v, e) }
    | v=VAR; LBRACKET; e1=expr; RBRACKET; ASSIGN; e2=expr                           { SAssign (LHArr (v, e1), e2) }
    | IF; e=expr; LBRACE; s1=stmt_list; RBRACE; ELSE; LBRACE; s2=stmt_list; RBRACE  { SIf (e, SBlock s1, SBlock s2) }
    | WHILE; e=expr; LBRACE; s=stmt_list; RBRACE                                    { SWhile (e, SBlock s) }
    | SKIP                                                                          { SSkip }
;

stmt_list:
    ss=separated_nonempty_list(SEMICOLON, stmt)     { ss }
;

const_list:
    cs=separated_nonempty_list(COMMA, const)        { cs }
;
