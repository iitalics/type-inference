#lang s-exp "lang.rkt"

1                          ; -> 1 : Int
2                          ; -> 2 : Int
(add1 3)                   ; -> 4 : Int
(+ 5 3)                    ; -> 8 : Int
(nat-rec {Int} 4 add1 5)   ; -> 9 : Int
(lambda ([x : Int]) "hi")       ; #<proc> : (→ Int Str)
(lambda ([x : Int]) x)          ; #<proc> : (→ Int Int)
(ann (lambda (x) x) : (→ Str Str)) ; #<proc> : (→ Str Str)


(nat-rec {Int}
         0
         (lambda (x) (+ x 2))
         5)                ; -> 10

(nat-rec {Int}
         1
         (lambda (x) (+ x x))
         5)                ; -> 32

(with-rand 10 (lambda (k) "hello")) ; -> "hello" : Str
