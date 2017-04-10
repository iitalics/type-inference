#lang s-exp "lang.rkt"

; datum
1                          ; -> 1 : Int
"hi"                       ; -> "hi" : Str

; simple application
(add1 3)                   ; -> 4 : Int
(+ 5 3)                    ; -> 8 : Int

; lambdas
(lambda ([x : Int]) "hi")       ; #<proc> : (→ Int Str)
(lambda ([x : Int]) x)          ; #<proc> : (→ Int Int)
(ann (lambda (x) x) : (→ Str Str)) ; #<proc> : (→ Str Str)

; polymorphic instantiation
(nat-rec {Int}
         1
         (lambda (x) (+ x x))
         5)                ; -> 32 : Int

; polymorphic inference (bidirectional)
(nat-rec 1
         (lambda ([x : Int]) (+ x x))
         5)                ; -> 32 : Int

; polymorphic inference (partial; bidirectional)
(with-rand 10 (lambda (x) "hi")) ; -> "hi" : Str
(with-rand 10 (lambda (x) (+ x x))) ; -> ... : Int

; TODO:
; polymorphic inference (backtracking; bidirectional)
#;(nat-rec 1
           (lambda (x) (+ x x))
           5)                ; -> 32 : Int
