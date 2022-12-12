type var = string

type const =
    | ENum of int
    | EBool of bool

type arr = const list

type value =
    | Const of const
    | Arr of arr

type bop =
    | Plus
    | Minus
    | Times
    | Div
    | And
    | Or
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
    | EArrIdx of var * exp

type stmt =
    | SAssign of var * exp
    | SArrAssign of var * arr
    | SArrIdxAssign of var * exp * exp
    | SIf of exp * stmt * stmt
    | SWhile of exp * stmt
    | SBlock of stmt list
    | SSkip
