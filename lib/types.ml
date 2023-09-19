open Ast
module VarMap = Map.Make (String)

type state = value VarMap.t
type ctx = typ VarMap.t
