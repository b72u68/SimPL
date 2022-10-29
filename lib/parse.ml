let parse filename =
    let chan = open_in filename in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.prog Lexer.token lexbuf in
    ast
;;

let parse_str s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.token lexbuf in
    ast
;;
