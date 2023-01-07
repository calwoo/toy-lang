open Core
open Toy.Parse_to_eval

let%expect_test "basic block" =
  parse_from_string "
    (block (* 2 3)
           (define x 42)
           (if (< 3 2)
               #t
               #f)
           (* 0 1)
           x)
  ";
  [%expect {|42|}]