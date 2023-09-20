open Ast

let _eval_bexpr = function
  | op, VConst (CInt n1), VConst (CInt n2) -> (
      match op with
      | Plus -> VConst (CInt (n1 + n2))
      | Minus -> VConst (CInt (n1 - n2))
      | Times -> VConst (CInt (n1 * n2))
      | Div -> VConst (CInt (n1 / n2))
      | Le -> VConst (CBool (n1 <= n2))
      | Lt -> VConst (CBool (n1 < n2))
      | Ge -> VConst (CBool (n1 >= n2))
      | Gt -> VConst (CBool (n1 > n2))
      | Eq -> VConst (CBool (n1 = n2))
      | Neq -> VConst (CBool (n1 <> n2))
      | _ -> failwith "Expected expression of type int")
  | op, VConst (CBool b1), VConst (CBool b2) -> (
      match op with
      | And -> VConst (CBool (b1 && b2))
      | Or -> VConst (CBool (b1 || b2))
      | Eq -> VConst (CBool (b1 = b2))
      | Neq -> VConst (CBool (b1 <> b2))
      | _ -> failwith "Expected expression of type bool")
  | _ -> failwith "Type mismatch for binary operation"

let rec _eval_expr (expr, state) =
  match expr.edesc with
  | EConst c -> VConst c
  | EArr cs -> VArr cs
  | EVar v -> Types.VarMap.find v state
  | EArrIdx (v, e) -> (
      let varr = Types.VarMap.find v state in
      let vconst = _eval_expr (e, state) in
      match (varr, vconst) with
      | VArr arr, VConst (CInt n) -> VConst (List.nth arr n)
      | _ -> failwith "Expected expression of type int")
  | EBinop (op, e1, e2) ->
      let v1 = _eval_expr (e1, state) in
      let v2 = _eval_expr (e2, state) in
      _eval_bexpr (op, v1, v2)
  | EIf (e, e1, e2) -> (
      let vcond = _eval_expr (e, state) in
      match vcond with
      | VConst (CBool cond) ->
          if cond then _eval_expr (e1, state) else _eval_expr (e2, state)
      | _ -> failwith "Expected expression of type bool")
  | EFun (FMax, [ e1; e2 ]) -> (
      let v1 = _eval_expr (e1, state) in
      let v2 = _eval_expr (e2, state) in
      match (v1, v2) with
      | VConst (CInt n1), VConst (CInt n2) -> VConst (CInt (max n1 n2))
      | _ -> failwith "Expected expression of type int")
  | EFun (FMin, [ e1; e2 ]) -> (
      let v1 = _eval_expr (e1, state) in
      let v2 = _eval_expr (e2, state) in
      match (v1, v2) with
      | VConst (CInt n1), VConst (CInt n2) -> VConst (CInt (min n1 n2))
      | _ -> failwith "Expected expression of type int")
  | EFun (FSize, [ e ]) -> (
      let varr = _eval_expr (e, state) in
      match varr with
      | VArr arr -> VConst (CInt (List.length arr))
      | _ -> failwith "Expected expression of type array")
  | _ -> failwith "Invalid expression"

let rec _arr_assign_idx arr idx value =
  if idx < 0 then failwith "Invalid index"
  else
    match arr with
    | [] -> failwith "Index out of bound"
    | h :: t ->
        if idx = 0 then value :: t else h :: _arr_assign_idx t (idx - 1) value

let rec small_step (stmt, state) =
  let loc = stmt.sloc in
  match stmt.sdesc with
  | SAssign (l, e) -> (
      let value = _eval_expr (e, state) in
      match l.ldesc with
      | LHVar v -> (mk_stmt SSkip loc, Types.VarMap.add v value state)
      | LHArr (v, e') -> (
          let idx = _eval_expr (e', state) in
          match idx with
          | VConst (CInt n) -> (
              let varr = Types.VarMap.find v state in
              match (varr, value) with
              | VArr arr, VConst c ->
                  let arr' = _arr_assign_idx arr n c in
                  (mk_stmt SSkip loc, Types.VarMap.add v (VArr arr') state)
              | _ -> failwith "Expected expression of type constant array")
          | _ -> failwith "Expected expression of type int"))
  | SIf (e, s1, s2) -> (
      let vcond = _eval_expr (e, state) in
      match vcond with
      | VConst (CBool b) -> if b then (s1, state) else (s2, state)
      | _ -> failwith "Expected expression of type bool")
  | SWhile (e, s) ->
      let t_s = mk_stmt (SBlock [ s; stmt ]) loc in
      let f_s = mk_stmt SSkip loc in
      (mk_stmt (SIf (e, t_s, f_s)) loc, state)
  | SBlock (s :: rest) -> (
      match s.sdesc with
      | SSkip -> (mk_stmt (SBlock rest) loc, state)
      | _ ->
          let s', state = small_step (s, state) in
          (mk_stmt (SBlock (s' :: rest)) loc, state))
  | SBlock [] -> (mk_stmt SSkip loc, state)
  | SSkip -> (mk_stmt SSkip loc, state)

let rec big_step (stmt, state) =
  match stmt.sdesc with
  | SSkip -> (stmt, state)
  | _ -> big_step @@ small_step (stmt, state)
