open Core
open Toy.Parse_to_eval

let%expect_test "fibonacci function" =
  parse_from_string "
    (define fib
      (lambda (n)
        (if (< n 3)
            1
            (+ (fib (- n 1)) (fib (- n 2))))))

    (fib 20)
  ";
  [%expect {|6765|}]