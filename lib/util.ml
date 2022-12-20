let rec string_of_type = function
    | Ast.TInt -> "int"
    | Ast.TBool -> "bool"
    | Ast.TArray t -> (string_of_type t) ^ " array"

let string_of_pos p =
    Lexing.(Printf.sprintf "%s:%d:%d"
            p.pos_fname
            p.pos_lnum
            (p.pos_cnum - p.pos_bol))
