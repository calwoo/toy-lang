open Core
open Parsing.Lexer
open Parsing.Parser
open Lexing
open Ast.Ast_expr
open Interp

(* Combine parser and lexer *)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try prog read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      value |> List.map ~f:expr_of_sexpr
      |> List.fold_left
           ~f:(fun acc e -> eval e acc.env)
           ~init:{ value = ValUnit; env = empty_env }
      |> fun recd -> recd.value |> value_to_string |> print_endline
  | None -> ()

let parse_from_file file =
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  parse_and_print lexbuf;
  In_channel.close inx

let parse_from_string prog_str =
  let lexbuf = Lexing.from_string prog_str in
  parse_and_print lexbuf
