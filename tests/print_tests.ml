open Core
open Toy.Parse_to_eval

let%expect_test "print lambda" =
  parse_from_string "
    (print (lambda (n) (+ n 1)))
  ";
  [%expect {|
    <lambda>
    #u
  |}]

let%expect_test "print named function" =
  parse_from_string "
    (define adder
      (lambda (x y)
        (+ x y)))
    
    (print adder)
  ";
  [%expect {|
    <func adder>
    #u
  |}]

let%expect_test "print primitive builtin function" =
  parse_from_string "(print +)";
  [%expect {|
    <prim fn>
    #u
  |}]
