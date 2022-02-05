type op = Plus | Minus | Times | Div | Lt | Gt | Le | Ge | Eq | And | Or
type var = string
type const = Int of int | True | False
type lst = const list
type exp =
    | Const of const
    | Var of var
    | Op of op * exp * exp
    | If of exp * exp * exp
    | Max of exp * exp
    | Min of exp * exp
    | Nth of lst * exp
    | Size of lst
