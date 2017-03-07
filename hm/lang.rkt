#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     "types.rkt"
                     "infer.rkt"))

(provide (rename-out [top-interaction #%top-interaction]))
(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(_ . t) #'(void)]))

(provide (rename-out [mod-begin #%module-begin]))
(define-syntax (mod-begin stx)
  (syntax-parse stx
    [(_ forms ...)
     (with-syntax
       ([(out ...)
         (for/list ([e (in-list (syntax-e #'(forms ...)))])
           (let*-values ([(e- e.t) (infer e)])
             #`(printf "~s : ~a => ~a\n"
                       '#,e
                       '#,e.t
                       '#,e-)))])
       #'(#%module-begin
          out ...))]))

(provide (rename-out [datum #%datum]))
(define-syntax (datum stx)
  (syntax-parse stx
    [(_ . d:integer)
     (bidirectional
      stx
      #:infer (lambda () (values #'(#%datum . d) (type-basic #'int))))]
    [(_ . d:str)
     (bidirectional
      stx
      #:infer (lambda () (values #'(#%datum . d) (type-basic #'string))))]
    ))

(provide (rename-out [lam lambda]))
(define-syntax (lam stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     (bidirectional
      stx
      #:infer (lambda ()
                (let*-values ([(x-) (generate-temporary #'x)]
                              [(x.t) (new-type)]
                              [(e- e.t) (infer* #'e (list #'x x.t x-))])
                  (values #`(lambda (#,x-) #,e-)
                          (type-fun x.t e.t)))))]
    ))
