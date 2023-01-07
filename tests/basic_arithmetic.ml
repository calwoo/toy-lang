open Core
open Toy.Parse_to_eval

let%expect_test "basic addition" =
  parse_from_string "(+ 2 3)";
  [%expect {|5|}]

let%expect_test "basic multiplication" =
  parse_from_string "(* 5 4)";
  [%expect {|20|}]

let%expect_test "nested arithmetic operators" =
  parse_from_string "(+ 2 3 (* 6 7))";
  [%expect {|47|}]

let%expect_test "multiline" =
  parse_from_string "
    (print (+ 2 3))
    (* 5 4)
  ";
  [%expect {|
    5
    20
  |}]
