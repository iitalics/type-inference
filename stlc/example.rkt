#lang s-exp "stlc.rkt"

1
2

"hello"

(lambda (x : A) x)

(lambda (x : A) (lambda (y : B) x))

(lambda (f : (A -> B))
  (lambda (x : A)
    (f x)))
