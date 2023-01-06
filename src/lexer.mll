{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let integer = '-'? ['0'-'9']+
let letters = ['a'-'z' 'A'-'Z']
let symbols = '+' | '-' | '*' | '/' | '!' | '?'
let id = (letters | symbols) (letters | digit | symbols)*

rule read = parse
    | white   { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | '('  { LPAREN }
    | ')'  { RPAREN }
    | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "#t" { BOOL true }
    | "#f" { BOOL false }
    | "#u" { UNIT }
    | id   { ID (Lexing.lexeme lexbuf) }
    | _    { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof  { EOF }
