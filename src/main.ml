let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.token lexbuf in
    ast
;;

let interp _ = ();;
