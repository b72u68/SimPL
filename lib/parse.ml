type parsesrc = FILE | STRING

let parse ?(source=FILE) (input: string) =
    let lexbuf =
        match source with
        | FILE -> open_in input |> Lexing.from_channel
        | STRING -> Lexing.from_string input
    in
    let ast = Parser.prog Lexer.token lexbuf in
    ast
;;
