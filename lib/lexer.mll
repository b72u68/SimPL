{
    open Parser
}

let digit = ['0'-'9']
let num = digit+
let varchar = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let var = ['a'-'z'] varchar*
let ws = [' ' '\t']

rule comment = parse
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | _ { comment lexbuf }
and token =  parse
    | ws { token lexbuf }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | "\r\n" { Lexing.new_line lexbuf; token lexbuf }
    | "\\" { comment lexbuf }
    | num as n { INT (int_of_string n) }
    | "true"  { TRUE }
    | "false" { FALSE }

    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { DIV }
    | "&&" { AND }
    | "||" { OR }
    | "<=" { LE }
    | "<" { LT }
    | ">=" { GE }
    | ">" { GT }
    | "!=" { NE }
    | "=" { EQ }
    | ":=" { ASSIGN }

    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "?" { QUESTION }
    | "," { COMMA }

    | "max" { MAX }
    | "min" { MIN }
    | "size" { SIZE }

    | "if" { IF }
    | "else" { ELSE }
    | "while" { WHILE }
    | "skip" { SKIP }

    | var as s { VAR s }

    | eof { EOF }
