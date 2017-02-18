#lang racket
(provide (rename-out
          [lang/module-begin #%module-begin]
          [lang/app #%app]
          [lang/datum #%datum]
          [lang/top #%top]
          [lang/lambda lambda]))
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


;;; typing context ;;;

(define-syntax-parameter
  typing-context
  '())

(define-syntax with-extend-context
  (syntax-parser
    #:datum-literals (= :)
    [(_ ([x : ty = e] ...) body ...)
     (with-syntax ([ctx (syntax-parameter-value #'typing-context)])
       #'(syntax-parameterize
             ([typing-context
               (list* (list #'x 'ty #'e) ... 'ctx)])
           body ...))]))

(define-for-syntax -> '())


;;;; typing continuation ;;;;

(define-syntax-parameter
  typing-cont
  (lambda (stx)
    (raise-syntax-error #f "no typing continuation" stx)))

(define-syntax lang/get-type
  (syntax-parser
    [(_ e:expr)
     (with-syntax ([k (syntax-parser
                        [(_ ty e-)
                         #''ty])])
       #'(syntax-parameterize ([typing-cont k])
           e))]))

(define-syntax lang/get-expanded
  (syntax-parser
    [(_ e:expr)
     (with-syntax ([k (syntax-parser
                        [(_ ty e-)
                         #''e-])])
       #'(syntax-parameterize ([typing-cont k])
           e))]))


;;; datum ;;;

(define-syntax lang/datum
  (syntax-parser
    [(_ . k:number)
     #'(typing-cont number (#%datum . k))]
    [(_ . s:str)
     #'(typing-cont string (#%datum . s))]))


;;; variable ;;;

(define-syntax lang/top
  (syntax-parser
    [(_ . x)
     (match (assf (lambda (y)
                    (cond
                      [(symbol? y)
                       (free-identifier=? #'x (datum->syntax #'x y))]
                      [else
                       (free-identifier=? #'x y)]))
                  (syntax-parameter-value #'typing-context))
       [(list _ type x-)
        (with-syntax ([type type]
                      [x- x-])
          #'(typing-cont type x-))]
       [#f
        (raise-syntax-error (syntax-e #'x) "no such variable!" #'x)])]))


;;; lambda ;;;

(define-syntax lang/lambda
  (syntax-parser
    #:datum-literals (:)
    [(_ (x : t) e)
     (let ([k-out (syntax-parameter-value #'typing-cont)])
       (with-syntax*
         ([(x-) (generate-temporaries #'(x))]
          [k (syntax-parser
               [(_ t- e-)
                (k-out #'(_ (t -> t-)
                            (lambda (x-) e-)))])])
         #'(syntax-parameterize ([typing-cont k])
             (with-extend-context ([x : t = x-])
               e))))]))


;;; application ;;;

(define-syntax lang/app
  (syntax-parser
    [(_ fn arg)
     (let ([k-out (syntax-parameter-value #'typing-cont)])
       (with-syntax*
         ([fn-k (lambda (arg-t arg-)
                  (syntax-parser
                    #:literals (->)
                    [(_ (exp-arg-t -> ret-t) fn-)
                     (unless (equal? arg-t
                                     (syntax->datum #'exp-arg-t))
                       (raise-syntax-error
                        #'arg (format "expected type ~s, got ~s"
                                      (syntax->datum #'exp-arg-t)
                                      arg-t)
                        #'arg))
                     (with-syntax ([arg- arg-])
                       (k-out #'(_ ret-t
                                   (#%app fn- arg-))))]
                    [(_ t fn-)
                     (raise-syntax-error
                      #'fn "calling value that is not a function"
                      #'fn)]))]
          [arg-k (syntax-parser
                   [(_ arg-t arg-)
                    #'(syntax-parameterize ([typing-cont
                                             (fn-k 'arg-t #'arg-)])
                        fn)])])
         #'(syntax-parameterize ([typing-cont arg-k])
             arg)))]))
