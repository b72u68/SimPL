type op =
    | Plus
    | Minus
    | Times
    | Div
    | Lt
    | Gt
    | Le
    | Ge
    | Eq
    | Neq
    | And
    | Or
    | Max
    | Min

type var = string

type const = Int of int | True | False

type lst = const list

type exp =
    | Const of const
    | Var of var
    | Op of op * exp * exp
    | IfExp of exp * exp * exp
    | Nth of var * exp
    | Size of lst

type value =
    | Exp of exp
    | Lst of lst

type stmt =
    | Let of var * value
    | LetNth of var * exp * exp
    | If of exp * stmt * stmt
    | While of exp * stmt
    | Seq of stmt list
    | Skip
