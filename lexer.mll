{
open Lexing
open Parser

exception SyntaxError of string

let syntaxError  s = raise (SyntaxError s)
let unexpected s = syntaxError ("lexer: Unexpected char: " ^ s)
let taberror ()  = syntaxError "lexer: Tab not allowed replace with spaces"

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let spaces      = [' ' '\r']+
let newline     = '\r' | '\n' | "\r\n"
let digit       = ['0'-'9']

rule read =
  parse
  | eof       { EOF }
  | '\t'      { taberror () }
  | newline   { next_line lexbuf; skipNewLines lexbuf}
  | spaces    { read lexbuf     }
  | "#"       { skipComment lexbuf }
  | digit+    { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { MUL   }
  | "/"       { DIV }
  | "("       {LPAR }
  | ")"       {RPAR }
  | _
                   { unexpected (Lexing.lexeme lexbuf ) }
and skipComment =
  parse
  | [^'\n'] { skipComment lexbuf }
  | _       { read lexbuf        }

and skipNewLines =
  parse
  | spaces   { skipNewLines lexbuf}
  | newline  { next_line   lexbuf; skipNewLines lexbuf }
  | eof      { EOF }
  | ""       { NEWLINE }
