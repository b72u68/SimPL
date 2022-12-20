open Util

type loc = Lexing.position * Lexing.position
type var = string

type typ =
    | TInt
    | TBool
    | TArray of typ

type const =
    | CInt of int
    | CBool of bool

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

type exp_ =
    | EConst of const
    | EVar of var
    | EArr of const list
    | EArrIdx of var * exp
    | EBinop of bop * exp * exp
    | EIf of exp * exp * exp
    | EFun of func * exp list
and exp = { edesc: exp_; eloc: loc }

type lhs_ =
    | LHVar of var
    | LHArr of var * exp
and lhs = { ldesc: lhs_; lloc: loc }

type stmt_ =
    | SAssign of lhs * exp
    | SIf of exp * stmt * stmt
    | SWhile of exp * stmt
    | SBlock of stmt list
    | SSkip
and stmt = { sdesc: stmt_; sloc: loc }

let mk_exp e loc = { edesc = e; eloc = loc }
let mk_lhs l loc = { ldesc = l; lloc = loc }
let mk_stmt s loc = { sdesc = s; sloc = loc }

let syn_err e s (spos, epos) =
    Printf.printf "%s--%s: Syntax Error: %s\n" (string_of_pos spos) (string_of_pos epos) s;
    raise e
