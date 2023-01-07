open Core
open Toy.Parse_to_eval

let%expect_test "equality check" =
  parse_from_string "(= 3 3)";
  [%expect {|#t|}]

let%expect_test "not equality check" =
  parse_from_string "(= 3 2)";
  [%expect {|#f|}]