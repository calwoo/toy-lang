open Core
open Toy.Parse_to_eval

let%expect_test "basic loop of lambda function" =
  parse_from_string "
    (define loop
      (lambda (start end f)
        (if (= start end)
            #u
            (block (print (f start))
                  (loop (+ start 1) end f)))))

    (loop 0 10 (lambda (x) (+ x 1)))
  ";
  [%expect {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
    #u
  |}]

let%expect_test "basic loop of named function" =
  parse_from_string "
    (define times2
      (lambda (x)
        (+ x x)))

    (define loop
      (lambda (start end f)
        (if (= start end)
            #u
            (block (print (f start))
                   (loop (+ start 1) end f)))))

    (loop 0 10 times2)
  ";
  [%expect {|
    0
    2
    4
    6
    8
    10
    12
    14
    16
    18
    #u
  |}]
