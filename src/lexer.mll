{
    open Parser
}

let digit = ['0'-'9']
let num = digit+
let varchar = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let var = ['a'-'z'] varchar*
let ws = [' ' '\t' '\n']

rule token =  parse
    | ws { token lexbuf }
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
    | "!=" { NEQ }
    | "==" { EQ }
    | "=" { EQUAL }

    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "?" { QUESTION }

    | "if" { IF }
    | "else" { ELSE }

    | "while" { WHILE }

    | "skip" { SKIP }

    | var as s { VAR s }

    | eof { EOF }
