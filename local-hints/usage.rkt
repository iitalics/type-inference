#lang s-exp "lang.rkt"

1                          ; -> 1 : Int
2                          ; -> 2 : Int
(add1 3)                   ; -> 4 : Int
(+ 5 3)                    ; -> 8 : Int
(nat-rec {Int} 4 add1 5)   ; -> 9 : Int
(lambda ([x : Int]) "hi")       ; #<proc> : (→ Int Str)
(lambda ([x : Int]) x)          ; #<proc> : (→ Int Int)
(ann (lambda (x) x) : (→ Str Str)) ; #<proc> : (→ Str Str)

(nat-rec 1
         (lambda ([x : Int]) (+ x x))
         5)                ; -> 32 : Int
