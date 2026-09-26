open Core
open Lexing
module Lexer = Stackmachine.Lexer
module Parser = Stackmachine.Parser
module Machine = Stackmachine.Machine
module Translate = Stackmachine.Translate
module Messages  = Stackmachine.Messages

(** Print the current position associated with the lexer buffer *)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** Print an error message and exit *)
let error lexbuf msg =
  fprintf stderr "%a: %s\n" print_position lexbuf msg;
  exit(-1)


let tryParsing lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg -> error lexbuf msg
  | Parser.Error st -> error lexbuf (Messages.message st)



let parse_and_compile lexbuf =
  let es = tryParsing lexbuf in          (* The parsing phase generates the expression list *)
  let prog = Translate.compile es in     (* Translate it to machine code *)
  let exec = Machine.Pretty.program prog in      (* Pretty print the executabe   *)
  print_endline exec


let process inx fname =
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  parse_and_compile lexbuf

let processFile fname =
  let inx = In_channel.create fname in
  process inx fname;
  In_channel.close inx

let processStdin () = process In_channel.stdin "-"


let () = let args = Sys.get_argv () in
         if Array.length args < 2
         then processStdin ()
         else processFile (args.(1))
