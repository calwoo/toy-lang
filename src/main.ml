open Core
open Lexer
open Lexing
open Ast
open Interp

(* Combine parser and lexer *)
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      value |> List.map ~f:expr_of_sexpr
      (* |> List.map ~f:expr_to_string
         |> String.concat ~sep:" "
         |> print_endline *)
      |> List.map ~f:(fun e -> eval e 0)
      |> List.map ~f:value_to_string
      |> String.concat ~sep:" " |> print_endline
  | None -> ()

let parse file =
  let inx = In_channel.create file in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  parse_and_print lexbuf;
  In_channel.close inx

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"Runtime for toy programs"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () -> parse filename))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
