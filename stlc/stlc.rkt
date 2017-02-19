#lang racket
(provide (rename-out
          [lang/module-begin #%module-begin]
          [lang/app #%app]
          [lang/datum #%datum]
          [lang/top #%top]
          [lang/lambda lambda]
          [lang/repl #%top-interaction]))
(require racket/stxparam
         (for-syntax syntax/parse
                     (only-in racket/match match)
                     (only-in racket/syntax with-syntax*)))

(define-syntax lang/module-begin
  (syntax-parser
    [(_ form ...)
     #'(#%module-begin
        (printf "expr: ~s\ntype: ~a\nexpanded: ~v\n\n"
                'form
                (lang/get-type form)
                (lang/get-expanded form)) ...)]))

(define-syntax lang/repl
  (syntax-parser
    [(_ . e)
     (with-syntax
       ([k (lambda (ty e-)
             (with-syntax ([ty ty] [e- e-])
               #'(printf "~a : ~s\n" e- 'ty)))])
       #'(syntax-parameterize ([typing-cont k])
           e))]))


;;; typing context ;;;

(define-syntax-parameter
  typing-context
  '())

(define-for-syntax -> '())


;;;; typing continuation ;;;;

(define-syntax-parameter
  typing-cont
  (lambda (ty e-)
    (raise-syntax-error #f "no typing continuation" e-)))

(define-for-syntax (get-typing-cont)
  (syntax-parameter-value #'typing-cont))

(define-syntax lang/get-type
  (syntax-parser
    [(_ e)
     (with-syntax ([k (lambda (ty e-)
                        (with-syntax ([ty ty])
                          (syntax/loc e- 'ty)))])
       #'(syntax-parameterize ([typing-cont k])
           e))]))

(define-syntax lang/get-expanded
  (syntax-parser
    [(_ e)
     (with-syntax ([k (lambda (ty e-)
                        (with-syntax ([e- e-])
                          #''e-))])
       #'(syntax-parameterize ([typing-cont k])
           e))]))


;;; datum ;;;

(define-syntax lang/datum
  (syntax-parser
    [(_ . k:number)
     ((get-typing-cont) 'number #'(#%datum . k))]
    [(_ . s:str)
     ((get-typing-cont) 'string #'(#%datum . s))]))


;;; variable ;;;

(define-syntax lang/top
  (syntax-parser
    [(_ . x)
     (match (assf (lambda (y)
                    (free-identifier=? #'x y))
                  (syntax-parameter-value #'typing-context))
       [(list _ type x-)
        ((get-typing-cont) type x-)]
       [#f
        (raise-syntax-error (syntax-e #'x) "no such variable!" #'x)])]))


;;; lambda ;;;

(define-syntax lang/lambda
  (syntax-parser
    #:datum-literals (:)
    [(_ (x : t) body)
     (let ([t (syntax->datum #'t)]
           [k-out (syntax-parameter-value #'typing-cont)]
           [ctx (syntax-parameter-value #'typing-context)])
       (with-syntax*
         ([(x-) (generate-temporaries #'(x))]
          [k (lambda (ret-t body-)
               (with-syntax ([body- body-])
                 (k-out `(,t -> ,ret-t)
                        #'(lambda (x-) body-))))]
          [new-ctx (lambda ()
                     (cons (list #'x t #'x-) ctx))])
         #'(syntax-parameterize ([typing-cont k]
                                 [typing-context (new-ctx)])
             body)))]))


;;; application ;;;

(define-syntax lang/app
  (syntax-parser
    [(_ fun arg)
     (let ([k-out (syntax-parameter-value #'typing-cont)])
       (with-syntax*
         ([k (lambda (arg-t arg-)
               (with-syntax
                 ([k2 (lambda (fun-t fun-)
                        (match fun-t
                          [`(,exp-arg-t -> ,ret-t)
                           (unless (equal? arg-t exp-arg-t)
                             (raise-syntax-error 'application
                                                 (format "expected type ~s, got ~s"
                                                         exp-arg-t arg-t)
                                                 #'arg))
                           (with-syntax ([fun- fun-] [arg- arg-])
                             (k-out ret-t
                                    #'(#%app fun- arg-)))]
                          [_
                           (raise-syntax-error 'application
                                               (format "not a function: ~s" fun-t)
                                               #'fun)]))])
                 #'(syntax-parameterize ([typing-cont k2])
                     fun)))])
         #'(syntax-parameterize ([typing-cont k])
             arg)))]))
