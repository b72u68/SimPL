open Ast
open Util

type ctx = typ VarMap.t

exception TypeError of string

let rec string_of_type = function
    | Ast.TInt -> "int"
    | Ast.TBool -> "bool"
    | Ast.TArray t -> (string_of_type t) ^ " array"

let typ_mismatch_err t1 t2 =
    Printf.sprintf "expected type %s, got %s" (string_of_type t1) (string_of_type t2)

let typ_err (spos, epos) msg =
    let err_msg = Printf.sprintf "%s--%s: %s"
                    (string_of_pos spos)
                    (string_of_pos epos)
                    msg
    in raise (TypeError err_msg)

let rec teq t1 t2 =
    match (t1, t2) with
    | (TInt, TInt) -> true
    | (TBool, TBool) -> true
    | (TArray t1, TArray t2) -> teq t1 t2
    | _ -> false

let rec typecheck_const = function
    | CInt _ -> TInt
    | CBool _ -> TBool

and typecheck_var ctx v loc =
    if VarMap.mem v ctx then VarMap.find v ctx
    else typ_err loc (Printf.sprintf "undefined variable \"%s\"" v)

and typecheck_exp ctx exp =
    match exp.edesc with
    | EConst c -> typecheck_const c
    | EVar v -> typecheck_var ctx v exp.eloc
    | EArr arr ->
            if List.length arr = 0 then typ_err exp.eloc "empty array, expected at least 1 element in an array"
            else
                let expected_type = typecheck_const (List.hd arr) in
                List.iter
                    (fun elem ->
                        let elem_type = typecheck_const elem in
                        if not (teq elem_type expected_type) then
                            typ_err exp.eloc (typ_mismatch_err expected_type elem_type)
                        else ())
                    arr;
                TArray expected_type
    | EArrIdx (v, e) ->
            let v_type = typecheck_var ctx v exp.eloc in
            (match v_type with
            | TArray t ->
                let e_type = typecheck_exp ctx e in
                if not (teq e_type TInt) then typ_err e.eloc (typ_mismatch_err TInt e_type)
                else t
            | _ -> typ_err exp.eloc (Printf.sprintf "expected type array, got %s" (string_of_type v_type)))
    | EBinop (bop, e1, e2) ->
            let e1_type = typecheck_exp ctx e1 in
            let e2_type = typecheck_exp ctx e2 in
            (match bop with
            | Plus | Minus | Times | Div ->
                    if not (teq e1_type TInt) then typ_err e1.eloc (typ_mismatch_err TInt e1_type)
                    else
                        if not (teq e2_type TInt) then typ_err e2.eloc (typ_mismatch_err TInt e2_type)
                        else TInt
            | Le | Lt | Ge | Gt ->
                    if not (teq e1_type TInt) then typ_err e1.eloc (typ_mismatch_err TInt e1_type)
                    else
                        if not (teq e2_type TInt) then typ_err e2.eloc (typ_mismatch_err TInt e2_type)
                        else TBool
            | And | Or ->
                    if not (teq e1_type TBool) then typ_err e1.eloc (typ_mismatch_err TBool e1_type)
                    else
                        if not (teq e2_type TBool) then typ_err e2.eloc (typ_mismatch_err TBool e2_type)
                        else TBool
            | Eq | Neq ->
                    if not (teq e1_type e2_type) then typ_err e2.eloc (typ_mismatch_err e1_type e2_type)
                    else TBool)
    | EIf (e1, e2, e3) ->
            let e1_type = typecheck_exp ctx e1 in
            if not (teq e1_type TBool) then typ_err e1.eloc (typ_mismatch_err TBool e1_type)
            else
                let e2_type = typecheck_exp ctx e2 in
                let e3_type = typecheck_exp ctx e3 in
                if not (teq e2_type e3_type) then typ_err e3.eloc (typ_mismatch_err e2_type e3_type)
                else e2_type
    | EFun (f, es) ->
            match f with
            | FMax | FMin ->
                    (match es with
                    | [e1; e2] ->
                            let e1_type = typecheck_exp ctx e1 in
                            if not (teq e1_type TInt) then typ_err e1.eloc (typ_mismatch_err TInt e1_type)
                            else
                                let e2_type = typecheck_exp ctx e2 in
                                if not (teq e2_type TInt) then typ_err e2.eloc (typ_mismatch_err TInt e2_type)
                                else TInt
                    | _ ->
                            let msg = Printf.sprintf "function expected 2 arguments, got %d" (List.length es) in
                            syn_err msg exp.eloc)
            | FSize ->
                    (match es with
                    | [e] ->
                            let e_type = typecheck_exp ctx e in
                            (match e_type with
                            | TArray _ -> TInt
                            | _ -> typ_err e.eloc (Printf.sprintf "expected type array, got %s" (string_of_type e_type)))
                    | _ ->
                            let msg = Printf.sprintf "function expected 1 arguments, got %d" (List.length es) in
                            syn_err msg exp.eloc)

and typecheck_stmt ctx s =
    match s.sdesc with
    | SAssign (lh, e) ->
            let e_type = typecheck_exp ctx e in
            (match lh.ldesc with
            | LHVar v ->
                    (try
                        let v_type = VarMap.find v ctx in
                        if not (teq v_type e_type) then typ_err e.eloc (typ_mismatch_err v_type e_type)
                        else ctx
                    with Not_found -> VarMap.add v e_type ctx)
            | LHArr (v, e') ->
                    let v_type = typecheck_var ctx v lh.lloc in
                    match v_type with
                    | TArray t ->
                            let e'_type = typecheck_exp ctx e' in
                            if not (teq e'_type TInt) then typ_err e'.eloc (typ_mismatch_err TInt e'_type)
                            else
                                if not (teq t e_type) then typ_err e.eloc (typ_mismatch_err t e_type)
                                else ctx
                    | _ -> typ_err e.eloc (Printf.sprintf "expected type array, got %s" (string_of_type e_type)))
    | SIf (e, s1, s2) ->
            let e_type = typecheck_exp ctx e in
            if not (teq e_type TBool) then typ_err e.eloc (typ_mismatch_err TBool e_type)
            else
                let ctx = typecheck_stmt ctx s1 in
                typecheck_stmt ctx s2
    | SWhile (e, s) ->
            let e_type = typecheck_exp ctx e in
            if not (teq e_type TBool) then typ_err e.eloc (typ_mismatch_err TBool e_type)
            else typecheck_stmt ctx s
    | SBlock ss -> List.fold_left typecheck_stmt ctx ss
    | SSkip -> ctx

let typecheck = typecheck_stmt (VarMap.empty)
