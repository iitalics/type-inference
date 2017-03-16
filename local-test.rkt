#lang s-exp "local.rkt"

int
(-> int int)
(->p (X) X X)
(->p (X) (->p (X) X) X)
