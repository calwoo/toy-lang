open Core
open Toy.Parse_to_eval

let%expect_test "list construction" =
  parse_from_string "(list 1 2 3)";
  [%expect {|(1, 2, 3)|}]

let%expect_test "list? check" =
  parse_from_string "(list? (list 2 3 4))";
  [%expect {|#t|}]

let%expect_test "list? check fail" =
  parse_from_string "(list? 5)";
  [%expect {|#f|}]
