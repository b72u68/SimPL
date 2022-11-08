type parsesrc = FILE | STRING

let parse ?(source=FILE) (input: string) =
    match source with
    | FILE ->
            let chan = open_in input in
            (try
                let lexbuf = Lexing.from_channel chan in
                let ast = Parser.prog Lexer.token lexbuf in
                close_in chan;
                ast
            with e ->
                close_in_noerr chan;
                raise e)
    | STRING ->
            let lexbuf = Lexing.from_string input in
            Parser.prog Lexer.token lexbuf
;;
