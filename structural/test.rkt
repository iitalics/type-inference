#lang racket
(require racket/stxparam)

(define-syntax-parameter counter #f)

(define-syntax-rule (with-counter body ...)
  (syntax-parameterize ([counter (box 0)])
    body ...))

(define-syntax (counter! stx)
  (syntax-case stx ()
    [(_)
     (let ([c-box (syntax-parameter-value #'counter)])
       (unless c-box
         (raise-syntax-error #f "not in a with-counter block" stx))
       (set-box! c-box
                 (add1 (unbox c-box)))
       (with-syntax ([v (unbox c-box)])
         #'v))]))


(with-counter
  (printf "~a\n" (counter!))
  (for ([_ (in-range 4)])
    (printf "-> ~a\n" (counter!)))
  (printf "~a\n" (counter!)))
