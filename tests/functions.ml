open Core
open Toy.Parse_to_eval

let%expect_test "basic lambda check" =
  parse_from_string "
    (define f
      (lambda (n)
        (* n (+ n 1))))

    (print (f 3))
  ";
  [%expect {|
    12
    #u
  |}]

let%expect_test "multiargument lambda function" =
  parse_from_string "
    (define adder
      (lambda (x y)
        (+ x y)))

    (adder 3 2)
  ";
  [%expect {|5|}]

let%expect_test "lambda application" =
  parse_from_string "((lambda (n) (* n n)) 3)";
  [%expect {|9|}]

let%expect_test "testing function closure" =
  parse_from_string "
    (define f
      (lambda (x y) (+ x y)))
    (define g
      (lambda (x) (f x x)))

    (g 3)
  ";
  [%expect {|6|}]

let%expect_test "testing function closure" =
  parse_from_string "
    (define y 2)

    (define addy
        (lambda (x)
            (+ x y)))
    
    (addy 5)
  ";
  [%expect {|7|}]

let%expect_test "argument binding" =
  parse_from_string "
    (define x 3)

    (define f
        (lambda (x)
            (+ x x)))
    
    (f 2)
  ";
  [%expect {|4|}]


let%expect_test "ignore shadowing variable in function closure" =
  parse_from_string "
    (define x 3)

    (define f
        (lambda (n)
            (+ n x)))

    (define x 5)
    
    (f 2)
  ";
  [%expect {|5|}]

let%expect_test "recursive function" =
  parse_from_string "
    (define factorial
      (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))

    (factorial 10)
  ";
  [%expect {|3628800|}]