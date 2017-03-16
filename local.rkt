#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  (require racket)

  (define tag:ask-parse-type 'parse-type-pls)
  (define tag:tell-parse-type 'parse-type-ok)
  (define tag:ask-check-type 'check-type-pls)
  (define tag:tell-check-type 'check-type-ok)
  (define tag:the-type 'the-type)
  (define tag:the-expr 'the-expr)

  (define (id-only-transformer fn)
    (make-set!-transformer
     (lambda (stx)
       (syntax-parse stx
         [_:id (fn)]
         [((~literal set!) _ _)
          (raise-syntax-error #f "invalid set! target" stx)]
         [(fn args ...)
          (with-syntax ([app (datum->syntax stx '#%app)])
            #'(app fn args ...))]))))

  (define (stx-props stx . l)
    (let trav ([stx stx]
               [l l])
      (match l
        [(list* k v l-)
         (trav (syntax-property stx k v) l-)]
        [_ stx])))

  (define (out:parse-type ty)
    (stx-props #'(quote 0)
               tag:tell-parse-type #t
               tag:the-type ty))

  (define (parse-type stx [wrap identity])
    (let* ([stx- (local-expand (stx-props (wrap stx)
                                          tag:ask-parse-type #t)
                               'expression
                               '())]
           [stx/t (trim-syntax stx-)])
      (cond
        [(syntax-property stx/t tag:tell-parse-type)
         (syntax-property stx/t tag:the-type)]
        [else
         (raise-syntax-error #f "no type parsed..." stx)])))

  (define (introduce-syntax bs)
    (lambda (stx0)
      (foldr (lambda (b stx)
               #`(let-syntax ([#,(car b) #,(cdr b)])
                   #,stx))
             stx0
             bs)))

  (define (introduce-syntax* . bs)
    (introduce-syntax bs))

  (define (trim-syntax stx)
    (syntax-parse stx
      [((~literal let-values) () stx-)
       (trim-syntax #'stx-)]
      [_ stx]))
  )

;; function type (not polymorphic)
(provide (rename-out [lang/-> ->]))
(define-syntax (lang/-> stx)
  (syntax-parse stx
    [(_ args ... ret)
     (with-syntax
       ([(args.t ...) (map parse-type
                           (syntax->list #'(args ...)))]
        [ret.t (parse-type #'ret)])
       (out:parse-type #'(--> {} args.t ... ret.t)))]))

(provide (rename-out [lang/->p ->p]))
(define-syntax (lang/->p stx)
  (syntax-parse stx
    [(_ (vars ...) args ... ret)
     (let* ([new-vars (generate-temporaries #'(vars ...))]
            [transformers (map (lambda (nv)
                                 (id-only-transformer
                                  (lambda () (out:parse-type nv))))
                               new-vars)]
            [bindings (map cons
                           (syntax->list #'(vars ...))
                           transformers)]
            [introduce (introduce-syntax bindings)])
       (with-syntax
         ([(args.t ...) (map (lambda (arg)
                               (parse-type arg introduce))
                             (syntax->list #'(args ...)))]
          [(vars.t ...) new-vars]
          [ret.t (parse-type #'ret introduce)])
         (out:parse-type #'(--> (vars.t ...)
                                args.t ...
                                ret.t))))]))

(provide (rename-out [lang/int int]))
(define-syntax lang/int
  (id-only-transformer
   (lambda ()
     (out:parse-type #'int))))

(provide (rename-out [lang/str str]))
(define-syntax lang/str
  (id-only-transformer
   (lambda ()
     (out:parse-type #'str))))

(provide (rename-out [lang/mod-begin #%module-begin]))
(define-syntax (lang/mod-begin stx)
  (syntax-parse stx
    [(_ forms ...)
     (with-syntax
       ([(ty ...) (map parse-type
                        (syntax->list #'(forms ...)))])
       #'(#%module-begin
          (printf "=> ~a\n"
                  'ty) ...))]))

(provide (rename-out [lang/repl #%top-interaction]))
(define-syntax (lang/repl stx)
  (syntax-parse stx
    [(_ . s)
     (with-syntax
       ([ty (parse-type #'s)])
       #'(printf "=> ~a\n" 'ty))]))
