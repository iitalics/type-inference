#lang racket
(require (for-syntax racket
                     racket/syntax
                     syntax/parse)
         (for-template syntax/parse))


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
         (unless (prop τ '#%type)
           (raise-syntax-error #f "not a type" src))
         τ)]))

  )

(define-syntax define-type
  (syntax-parser

    ;;; basic type ;;;
    [(_ x:id)
     #:with x/pat (format-id #'x "~~~a" (syntax-e #'x))
     #:with x/int (generate-temporary #'x)
     #'(begin
         ; internal representation
         (define (x/int . _) (error "evaluating type at runtime"))

         ; external transformer
         (define-syntax x
           (make-set!-transformer
            (syntax-parser
              [_:id
               #:fail-unless (eq? (prop this-syntax 'ctx) 'type-eval) "type used in wrong context"
               (attach #'(#%app x/int) '#%type #t)]
              [(a . b)
               #:fail-when #t "type takes no arguments"
               #f])))

         ; pattern expander
         (begin-for-syntax
           (define-syntax x/pat
             (pattern-expander
              (lambda (s) (syntax-case s ()
                       [_ #'((~literal #%plain-app)
                             (~literal x/int))]))))))]

    ;;; constructor ;;;
    [(_ x:id #:arity ar-pred)
     #:with x/pat (format-id #'x "~~~a" (syntax-e #'x))
     #:with x/int (generate-temporary #'x)
     #'(begin
         ; internal representation
         (define (x/int . _) (error "evaluating type at runtime"))

         ; external transformer
         (define-syntax x
           (let ([ar? ar-pred])
             (syntax-parser
               [(_ ts (... ...))
                #:fail-unless (eq? (prop this-syntax 'ctx) 'type-eval) "type used in wrong context"
                #:fail-unless (ar? (length (syntax-e #'(ts (... ...)))))
                (format "wrong number of arguments to type ~a" 'x)
                #:with (τs (... ...)) (map type-eval (syntax-e #'(ts (... ...))))
                (attach #'(x/int (lambda () τs (... ...))) '#%type #t)])))

         ; pattern expander
         (begin-for-syntax
           (define-syntax x/pat
             (pattern-expander
              (lambda (s) (syntax-case s ()
                       [(_ . ps) #'((~literal #%plain-app)
                                    (~literal x/int)
                                    . _)]))))))]
    ))


;;;; PROTOTYPES ;;;;

(define-type ??)



;;;; INFERING FROM MACROS ;;;;

(begin-for-syntax
  ;; infer the type of an expression
  ;; returns a list of the output expression, the type, and the bound variables
  ;; optionally provide a prototype or context
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
  (define --ctx-barrier-- (lambda (i) i))

  ;; wrap expression in a context
  ;; returns a new expression
  (define (wrap-ctx e ctx)
    (foldl (lambda (pr e)
             (match-let ([(cons x τ) pr])
               #`(lambda (#,x)
                   (let-syntax ([x (make-typed-alias (syntax #,x) (quote #,τ))])
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
        [((~literal #%plain-app)
          (~literal --ctx-barrier--) e-)
         (values #'e- xs)]
        [u (raise-syntax-error #f "unexpected syntax in trim" #'u)])))

  ;; returns a set! transformer that expands to the given syntax,
  ;; and has the given type
  (define (make-typed-alias e τ)
    (make-set!-transformer
     (syntax-parser
       [_:id (attach e 'inferred-type τ)]
       [(a . b)
        #'(app a . b)])))
  )

(define-syntax define-typed-alias
  (syntax-parser
    [(_ [e x-new] (~datum :) t)
     (let ([τ (type-eval #'t)])
       #`(define-syntax x-new
           (make-typed-alias #'e (quote #,τ))))]))



;;;; THE ACTUAL LANGUAGE ;;;;

(define-type Int)
(define-type Str)
(define-type → #:arity (>=/c 1))

(provide Int Str →)

(define msg "hello")
(define-typed-alias [msg msg/t] : Str)
(provide (rename-out [msg/t msg]))

(define-typed-alias [+ plus] : (→ Int Int Int))
(provide (rename-out [plus +]))


(provide (rename-out [module-begin #%module-begin]))
(define-syntax module-begin
  (syntax-parser
    [_ #'(#%module-begin (displayln "--- Hello --"))]))

(provide (rename-out [top-eval #%top-interaction]))
(define-syntax top-eval
  (syntax-parser
    [(_ . e)
     #:with (e- τ _) (infer #'e)
     #'(printf "~s : ~a\n"
               e- 'τ)]))

(provide (rename-out [dat #%datum]))
(define-syntax dat
  (syntax-parser
    [(_ . k:integer) (inferred #'(#%datum . k) #'Int)]
    [(_ . k:str)     (inferred #'(#%datum . k) #'Str)]
    [(_ . k) (raise-syntax-error #f "unsupported datum" #'k)]))

(provide :int)
(define-syntax :int
  (syntax-parser
    [(_ e)
     #:with (e- τ _) (infer #'e #:proto (type-eval #'Int))
     (syntax-parse #'τ
       [~Int #'e-]
       [_ (raise-syntax-error #f "not type int!" this-syntax)])]))

(provide :exp-type)
(define-syntax :exp-type
  (syntax-parser
    [(_ t)
     #:with τ (type-eval (type-eval #'t))
     #:do [(syntax-parse #'τ
             [(~→ x ...) (printf "its a function.\n")]
             [_ (void)])]
     (inferred #''τ
               #'??)]))

(provide (rename-out [app #%app]))
(define-syntax app
  (syntax-parser
    [(_ fun arg ...)
     #:with (fun- fun_τ _) (infer #'fun)
     #:do [(syntax-parse #'fun_τ
             [((~literal #%plain-app) _
               ((~literal #%plain-lambda) τs ...))
              (printf "; its a constructor of some kind.\n")]
             [(a . b)
              (printf "; first part of pair:\n~a\n" #'a)]
             [_ (printf "; it's nothing.")])]
     (inferred #''ok #'??)]))
