#lang s-exp "stlc.rkt"

; datum
1
2
"hello"

; lambda
(lambda (x : A) x)

; nested lambda
(lambda (x : A) (lambda (y : B) x))

; application
(lambda (f : (A -> B))
  (lambda (x : A)
    (f x)))

; let binding
(let (n : integer = 16)
  (* n n))

#|
; application with invalid argument
(lambda (f : (A -> B))
  (lambda (x : C)
    (f x)))

; application of non-function
(2 3)
|#
