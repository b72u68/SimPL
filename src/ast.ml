type binop =
    | Plus
    | Minus
    | Times
    | Div

type relop =
    | Lt
    | Gt
    | Le
    | Ge
    | Eq
    | Neq

type var = string

type intexpr =
    | Num of int
    | Var of var
    | Binop of binop * intexpr * intexpr
    | IfExpr of boolexpr * intexpr * intexpr

and boolexpr =
    | True
    | False
    | Relop of relop * intexpr * intexpr
    | And of boolexpr * boolexpr
    | Or of boolexpr * boolexpr

type stmt =
    | Assign of var * intexpr
    | If of boolexpr * stmt * stmt
    | While of boolexpr * stmt
    | Seq of stmt list
    | Skip
