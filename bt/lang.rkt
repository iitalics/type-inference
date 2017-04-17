#lang racket
(require (for-syntax racket
                     racket/syntax
                     syntax/parse)
         (for-meta -1 syntax/parse)
         (for-meta 2 syntax/parse))


;;;; LOW LEVEL TYPE / SYNTAX UTILITIES ;;;;

(begin-for-syntax
  (define (attach t . l)
    (let att ([t t] [l l])
      (match l
        [(list* k v l-) (att (syntax-property t k v) l-)]
        [_ t])))

  (define prop syntax-property)

  ;; evaluate a type into its expanded form
  (define (type-eval t #:src [src t])
    (cond
      [(prop t '#%type) t]
      [else
       (let* ([t* (attach t 'ctx 'type-eval)]
              [τ (local-expand t* 'expression '())])
         #;(unless (prop τ '#%type)
           (raise-syntax-error #f "not a type" src))
         τ)]))

  )

(define-syntax define-base-type
  (syntax-parser
    [(_ B:id)
     #:with B/pat (format-id #'B "~~~a" (syntax-e #'B))
     #:with B/int (format-id #'B "internal:~a" (syntax-e #'B))
     #'(begin
         (begin-for-syntax
           (define-syntax B/pat
             (pattern-expander
              (syntax-parser
                [_ #'((~literal #%plain-app)
                      (~literal B/int))]))))

         (define (B/int)
           (error "use of type" 'B "as expression"))
         (define-syntax (B -stx-)
           (syntax-case -stx- ()
             [(_ . _) (raise-syntax-error #f "type doesn't take arguments" -stx-)]
             [_ (attach #'(#%app B/int) '#%type #t)]))
         )]))

(define-syntax define-ctor-type
  (syntax-parser
    [(_ B:id . _)
     #:with B/pat (format-id #'B "~~~a" (syntax-e #'B))
     #:with B/int (format-id #f "internal:~a" (syntax-e #'B))
     #'(begin
         (begin-for-syntax
           (define-syntax B/pat
             (pattern-expander
              (syntax-parser
                [(_ . p) #'((~literal #%plain-app)
                            (~literal B/int)
                            (#%plain-lambda () . p))]))))

         (define (B/int)
           (error "use of type" 'B "as expression"))
         (define-syntax (B -stx-)
           (syntax-case -stx- ()
             [(_ . args)
              (attach #'(#%app B/int (lambda () . args))
                      '#%type #t)]))
         )]))



;;;; TYPE INFERENCE MECHANICS ;;;;

(begin-for-syntax
  (define (infer e
                 #:ctx [ctx-alist '()]
                 #:proto [π (type-eval #'??)])
    (let* ([e* (wrap-ctx (attach e 'prototype π) ctx-alist)]
           [e~ (local-expand e* 'expression '())])
      (let*-values ([(e- xs-) (trim-syntax e~)]
                    [(τ) (prop e- 'inferred-type)])
        (unless τ
          (raise-syntax-error #f
                              "no type inferred (perhaps no checking rules defined for expressio)"
                              e))
        ;; TODO: check that inferred-type is compatible with the prototype
        (list e- τ xs-))))

  ;; get prototype of expression (for inference)
  (define (prototypeof e)
    (or (prop e 'prototype)
        (type-eval #'??)))

  ;; attach inferred type
  (define (inferred e t)
    (attach e 'inferred-type (type-eval t)))

  ;; this is used to seperate expressions in introduced context
  ;; from resulting expressions
  (define --ctx-barrier-- (lambda (i)
                            (printf "== leftover context barrier! ==\n")
                            i))

  ;; wrap expression in a context
  ;; returns a new expression
  (define (wrap-ctx e ctx)
    (foldl (lambda (pr e)
             (match-let ([(cons x τ) pr])
               #`(lambda (#,x)
                   (let-syntax ([#,x (make-typed-alias (syntax #,x) (quote #,τ))])
                     #,e))))
           #`(#%plain-app --ctx-barrier-- #,e)
           ctx))

  ;; trim context barrier
  ;; returns two values, the inner expression, and a list of all
  ;; the bound identifiers
  (define (trim-syntax e)
    (let trav ([e e] [xs '()])
      (syntax-parse e
        [((~literal #%plain-lambda) (x:id) e-)  (trav #'e- (cons #'x xs))]
        [((~literal let-values) () e-)          (trav #'e- xs)]
        [((~literal #%expression) e-)           (trav #'e- xs)]
        [((~literal #%plain-app) (~literal --ctx-barrier--) e-)
         (values #'e- xs)]
        [u (raise-syntax-error #f "unexpected syntax in trim" #'u)])))

  ;; returns a set! transformer that expands to the given syntax,
  ;; and has the given type
  (define (make-typed-alias e τ)
    (make-set!-transformer
     (syntax-parser
       [_:id (inferred e τ)]
       [(a . b) #'(app a . b)])))
  )






;;;; ACTUAL LANGUAGE ;;;;

(provide (rename-out [mod #%module-begin]))
(define-syntax mod
  (syntax-parser
    [(_ . _) #'(#%module-begin (printf "hello.\n"))]))


(define-base-type Int)
(define-ctor-type ->)

(provide (rename-out [top #%top-interaction]))
(define-syntax top
  (syntax-parser
    [(_ . e)
     #:with (~-> t ...) (type-eval #'(-> Int Int))
     #'(displayln '(t ...))]))
