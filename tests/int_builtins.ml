open Core
open Toy.Parse_to_eval

let%expect_test "int check" =
  parse_from_string "
    (print (int? 3))
    (print (int? #t))
  ";
  [%expect {|
    #t
    #f
    #u
  |}]

let%expect_test "zero test" =
  parse_from_string "
    (print (zero? 3))
    (zero? 0)
  ";
  [%expect {|
    #f
    #t
  |}]
