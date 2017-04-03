#lang s-exp "lang.rkt"

1                          ; -> 1
2                          ; -> 2
(add1 3)                   ; -> 4
(+ 5 3)                    ; -> 8
(nat-rec-int 4 add1 5)     ; -> 9
(lambda ([x : Int]) "hi")       ; #<proc> : (→ Int Str)
(lambda ([x : Int]) x)          ; #<proc> : (→ Int Int)

(nat-rec-int 0 (lambda (x)
                 (+ x 2))
             5)            ; -> 10

(nat-rec-int 1 (lambda (x)
                 (+ x x))
             5)            ; -> 32
