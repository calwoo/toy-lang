open Core
open Toy.Parse_to_eval

let%expect_test "basic mutual recursion" =
  parse_from_string "
    (define even?
      (lambda (n)
        (if (= n 0)
             #t
             (odd? (- n 1)))))

    (define odd?
      (lambda (n)
        (if (= n 1)
            #t
            (even? (- n 1)))))

    (print (even? 42))
    (odd? 42)
  ";
  [%expect {|
    #t
    #f
    #u
  |}]
