let parse s =
    let lexbuf = Lexing.from_string s in
    Parser.expr Lexer.token lexbuf
;;
