open Core
open Toy.Parse_to_eval

let%expect_test "basic define check" =
  parse_from_string "
    (define x (+ 2 3))
    (print (* x x))
  ";
  [%expect {|
    25
    #u
  |}]