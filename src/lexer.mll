{
open Parser
}

let white = [' ' '\t']
let digit = ['0'-'9']
let integer = '-'? ['0'-'9']+
let id = ['a'-'z' 'A'-'Z']+

rule lex = parse
    | white { read lexbuf }
    | '('   { LPAREN }
    | ')'   { RPAREN }
    | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "#t" { BOOL true }
    | "#f" { BOOL false }
    | "#u" { UNIT }
    | id   { ID (Lexing.lexeme lexbuf) }
    | eof  { EOF }
