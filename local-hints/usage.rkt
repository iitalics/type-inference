#lang s-exp "lang.rkt"

1                          ; -> 1
2                          ; -> 2
(add1 3)                   ; -> 4
(add2 4)                   ; -> 6
(+ 5 3)                    ; -> 8
(nat-rec-int 0 add2 5)     ; -> 10
