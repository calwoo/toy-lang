open Core
open Toy.Parse_to_eval

let%expect_test "higher order apply" =
  parse_from_string "
    (define add1
      (lambda (n)
        (+ n 1)))

    (define apply
      (lambda (f x)
        (f x)))

    (apply add1 10)
  ";
  [%expect {|11|}]

let%expect_test "higher order apply a recursive function" =
  parse_from_string "
    (define factorial
      (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))

    (define apply
      (lambda (f x)
        (f x)))

    (apply factorial 10)
  ";
  [%expect {|3628800|}]
