open Core
open Toy.Parse_to_eval

let%expect_test "Test name" =
  parse_from_string "(+ 2 3)";
  [%expect {|5|}]

let%expect_test "Test name" =
  parse_from_string "(+ 2 3 (* 6 7))";
  [%expect {|47|}]
