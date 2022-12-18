type var = string

type typ =
    | TInt
    | TBool
    | TArray of typ

type const =
    | EInt of int
    | EBool of bool

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
    | EArr of const list
    | EArrIdx of var * exp
    | EBinop of bop * exp * exp
    | EIf of exp * exp * exp
    | EFun of func * exp list

type lhs =
    | LHVar of var
    | LHArr of var * exp

type stmt =
    | SAssign of lhs * exp
    | SIf of exp * stmt * stmt
    | SWhile of exp * stmt
    | SBlock of stmt list
    | SSkip
