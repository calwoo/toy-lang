open Core
open Toy.Parse_to_eval

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"Runtime for toy programs"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () ->
         parse_from_file filename))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
