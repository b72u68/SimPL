open Ast
open Util

module VarMap = Map.Make(String)

type ctx = typ VarMap.t

exception TypeError of string

let typ_err t1 t2 pos =
    Printf.sprintf "%s: TypeError: expected type %s, got %s\n"
                    (string_of_pos pos)
                    (string_of_type t1)
                    (string_of_type t2)

let rec teq t1 t2 =
    match (t1, t2) with
    | (TInt, TInt) -> true
    | (TBool, TBool) -> true
    | (TArray t1, TArray t2) -> teq t1 t2
    | _ -> false

let rec typecheck_const = function
    | CInt _ -> TInt
    | CBool _ -> TBool

and typecheck_arr arr =
    if List.length arr = 0 then failwith "empty array"
    else
        let expected_type = typecheck_const (List.hd arr) in
        List.iter
            (fun elem ->
                let elem_type = typecheck_const elem in
                if not (teq elem_type expected_type) then
                    raise (TypeError (typ_err expected_type elem_type))
                else ())
            arr;
        TArray expected_type

and typecheck_var ctx v =
    try
        VarMap.find v ctx
    with Not_found -> raise (TypeError (Printf.sprintf "undefined variable \"%s\"" v))

and typecheck_exp ctx exp =
    match exp.edesc with
    | EConst c -> typecheck_const c
    | EVar v -> typecheck_var ctx v
    | EArr arr -> typecheck_arr arr
    | EArrIdx (v, e) ->
            let v_type = typecheck_var ctx v in
            (match v_type with
            | TArray t ->
                let e_type = typecheck_exp ctx e in
                if not (teq e_type TInt) then raise (TypeError (typ_err TInt e_type))
                else t
            | _ -> failwith "expected type array")
    | EBinop (bop, e1, e2) ->
            let e1_type = typecheck_exp ctx e1 in
            let e2_type = typecheck_exp ctx e2 in
            (match bop with
            | Plus | Minus | Times | Div | Le | Lt | Ge | Gt ->
                    if not (teq e1_type TInt) then raise (TypeError (typ_err TInt e1_type))
                    else
                        if not (teq e2_type TInt) then raise (TypeError (typ_err TInt e2_type))
                        else TInt
            | And | Or ->
                    if not (teq e1_type TBool) then raise (TypeError (typ_err TBool e1_type))
                    else
                        if not (teq e2_type TBool) then raise (TypeError (typ_err TBool e2_type))
                        else TBool
            | Eq | Neq ->
                    if not (teq e1_type e2_type) then raise (TypeError (typ_err e1_type e2_type))
                    else TBool)
    | EIf (e1, e2, e3) ->
            let e1_type = typecheck_exp ctx e1 in
            if not (teq e1_type TBool) then raise (TypeError (typ_err TBool e1_type))
            else
                let e2_type = typecheck_exp ctx e2 in
                let e3_type = typecheck_exp ctx e3 in
                if not (teq e2_type e3_type) then raise (TypeError (typ_err e2_type e3_type))
                else e2_type
    | EFun (f, es) ->
            match f with
            | FMax | FMin ->
                    (match es with
                    | [e1; e2] ->
                            let e1_type = typecheck_exp ctx e1 in
                            if not (teq e1_type TInt) then raise (TypeError (typ_err TInt e1_type))
                            else
                                let e2_type = typecheck_exp ctx e2 in
                                if not (teq e2_type TInt) then raise (TypeError (typ_err TInt e2_type))
                                else TInt
                    | _ -> raise (SyntaxError (Printf.sprintf "function expected 2 arguments, got %d" (List.length es))))
            | FSize ->
                    (match es with
                    | [e] ->
                            let e_type = typecheck_exp ctx e in
                            (match e_type with
                            | TArray _ -> TInt
                            | _ -> raise (TypeError (Printf.sprintf "expected type array, got %s" (string_of_type e_type))))
                    | _ -> raise (SyntaxError (Printf.sprintf "function expected 1 arguments, got %d" (List.length es))))

and typecheck_stmt ctx = function
    | SAssign (lh, e) ->
            let e_type = typecheck_exp ctx e in
            (match lh.ldesc with
            | LHVar v ->
                    (try
                        let v_type = typecheck_var ctx v in
                        if not (teq v_type e_type) then raise (TypeError (typ_err v_type e_type))
                        else ctx
                    with TypeError _ -> VarMap.add v e_type ctx)
            | LHArr (v, e') ->
                    let v_type = typecheck_var ctx v in
                    match v_type with
                    | TArray t ->
                            let e'_type = typecheck_exp ctx e' in
                            if not (teq e'_type TInt) then raise (TypeError (typ_err TInt e'_type))
                            else
                                if not (teq t e_type) then raise (TypeError (typ_err t e_type))
                                else ctx
                    | _ -> raise (TypeError (Printf.sprintf "expected type array, got %s" (string_of_type e_type))))
    | SIf (e, s1, s2) ->
            let e_type = typecheck_exp ctx e in
            if not (teq e_type TBool) then raise (TypeError (typ_err TBool e_type))
            else
                let ctx = typecheck_stmt ctx s1 in
                typecheck_stmt ctx s2
    | SWhile (e, s) ->
            let e_type = typecheck_exp ctx e in
            if not (teq e_type TBool) then raise (TypeError (typ_err TBool e_type))
            else typecheck_stmt ctx s
    | SBlock ss -> List.fold_left typecheck_stmt ctx ss
    | SSkip -> ctx

let typecheck = typecheck_stmt (VarMap.empty)
