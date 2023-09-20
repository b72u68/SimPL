open Ast

let rec repeat n s = if n <= 0 then "" else s ^ repeat (n - 1) s
let pad lvl = repeat lvl "\t"

let print_bop = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | And -> "&&"
  | Or -> "||"
  | Le -> "<="
  | Lt -> "<"
  | Ge -> ">="
  | Gt -> ">"
  | Eq -> "="
  | Neq -> "!="

let print_fun = function FMax -> "max" | FMin -> "min" | FSize -> "size"

let print_const = function
  | CInt n -> string_of_int n
  | CBool b -> string_of_bool b

let print_seq (to_str : 'a -> string) (arr : 'a list) (delim : string) : string
    =
  let rec print_rec = function
    | [] -> ""
    | [ h ] -> to_str h
    | h :: t -> Printf.sprintf "%s%s%s" (to_str h) delim (print_rec t)
  in
  print_rec arr

let print_value = function
  | VConst c -> print_const c
  | VArr cs -> Printf.sprintf "[ %s ]" @@ print_seq print_const cs "; "

let print_state state =
  let state_lst = List.of_seq (Types.VarMap.to_seq state) in
  let print_state_pair (var, value) =
    Printf.sprintf "%s : %s" var (print_value value)
  in
  "{ " ^ print_seq print_state_pair state_lst ", " ^ " }"

let rec print_expr expr =
  match expr.edesc with
  | EConst c -> print_const c
  | EArr cs -> Printf.sprintf "[ %s ]" @@ print_seq print_const cs "; "
  | EVar v -> v
  | EArrIdx (v, e) -> Printf.sprintf "%s[%s]" v @@ print_expr e
  | EBinop (op, e1, e2) ->
      Printf.sprintf "%s %s %s" (print_expr e1) (print_bop op) (print_expr e2)
  | EIf (e, e1, e2) ->
      Printf.sprintf "%s ? %s : %s" (print_expr e) (print_expr e1)
        (print_expr e2)
  | EFun (fn, args) ->
      Printf.sprintf "%s(%s)" (print_fun fn) (print_seq print_expr args ", ")

let print_lhs lhs =
  match lhs.ldesc with
  | LHVar v -> v
  | LHArr (v, e) -> Printf.sprintf "%s[%s]" v @@ print_expr e

let print_stmt stmt =
  let rec print_stmt_rec indent stmt =
    let indent_str = pad indent in
    match stmt.sdesc with
    | SAssign (l, e) ->
        Printf.sprintf "%s%s = %s" indent_str (print_lhs l) (print_expr e)
    | SIf (e, s1, s2) ->
        Printf.sprintf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}" indent_str
          (print_expr e)
          (print_stmt_rec (indent + 1) s1)
          indent_str
          (print_stmt_rec (indent + 1) s2)
          indent_str
    | SWhile (e, s) ->
        Printf.sprintf "%swhile (%s) {\n%s\n%s}" indent_str (print_expr e)
          (print_stmt_rec (indent + 1) s)
          indent_str
    | SBlock ss -> print_seq (print_stmt_rec indent) ss ";\n"
    | SSkip -> indent_str ^ "skip"
  in
  print_stmt_rec 0 stmt
