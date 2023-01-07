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

let%expect_test "fibonacci loop first 20" =
  parse_from_string "
    (define fib
      (lambda (n)
        (if (< n 3)
            1
            (+ (fib (- n 1)) (fib (- n 2))))))

    (define loop
      (lambda (start end f)
        (if (= start end)
            #u
            (block (print (f start))
                    (loop (+ start 1) end f)))))

    (loop 1 21 fib)
  ";
  [%expect {|
    1
    1
    2
    3
    5
    8
    13
    21
    34
    55
    89
    144
    233
    377
    610
    987
    1597
    2584
    4181
    6765
    #u
  |}]