open Core
open Toy.Parse_to_eval

let%expect_test "bool? check" =
  parse_from_string "
    (print (bool? 3))
    (print (bool? #t))
    (bool? #f)
  ";
  [%expect {|
    #f
    #t
    #t
  |}]

let%expect_test "not? check" =
  parse_from_string "
    (print (not #t))
    (not #f)
  ";
  [%expect {|
    #f
    #t
  |}]