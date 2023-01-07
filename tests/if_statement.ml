open Core
open Toy.Parse_to_eval

let%expect_test "basic if check" =
  parse_from_string "
    (if (= 3 2)
        3
        (+ 2 3))
  ";
  [%expect {|5|}]