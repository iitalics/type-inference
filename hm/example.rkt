#lang s-exp "lang.rkt"

(lambda (f g) (lambda (x) (f (g x))))
