type var = string

type const =
    | ENum of int
    | EBool of bool

type bop =
    | Plus
    | Minus
    | Times
    | And
    | Or
    | Div
    | Le
    | Lt
    | Ge
    | Gt
    | Eq
    | Neq

type func =
    | FMax
    | FMin
    | FSize

type exp =
    | EConst of const
    | EVar of var
    | EBinop of bop * exp * exp
    | EIf of exp * exp * exp
    | EFun of func * exp list

type stmt =
    | SAssign of var * exp
    | SIf of exp * stmt * stmt
    | SWhile of exp * stmt
    | SBlock of stmt list
    | SSkip
