type op = Plus | Minus | Times | Div | Lt | Gt | Le | Ge | Eq | And | Or
type var = string
type exp =
    | Int of int
    | Bool of bool
    | Var of var
    | Op of op * exp * exp
    | If of exp * exp * exp
    | Max of exp * exp
    | Min of exp * exp
