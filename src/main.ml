open Core

let parse file =
  In_channel.with_file file ~f:(fun ic ->
    In_channel.input_all ic |> print_endline)

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Runtime for toy programs"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () ->
      parse filename))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
