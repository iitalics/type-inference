#lang turnstile
(require (only-in racket/format ~a)
         (only-in racket/string string-join)
         (only-in racket/function identity const)
         racket/match
         syntax/parse
         (only-in racket/sequence
                  in-syntax))

(provide (type-out Int Str →))

(define-type-constructor → #:arity >= 1)
(define-binding-type ∀ #:arity = 1 #:bvs >= 1)
(define-base-types Int Str)
(define-base-type ??)

(begin-for-syntax
  (define-syntax-class base-type
    #:attributes (name internal-name)
    (pattern ((~literal #%plain-app) internal-name:id)
             #:with (_ ... name) (syntax-property this-syntax 'orig)))
  (define-syntax-class ctor-type
    #:attributes (name internal-name [arg 1])
    (pattern ((~literal #%plain-app) internal-name:id
              ((~literal #%plain-lambda) ()
               ((~literal #%expression) . _)
               arg ...))
             #:with (_ ... (name . _)) (syntax-property this-syntax 'orig)))
  (define-syntax-class bind-type
    #:attributes (name internal-name [bv 1] [arg 1])
    (pattern ((~literal #%plain-app) internal-name:id
              ((~literal #%plain-lambda) (bv:id ...)
               ((~literal #%expression) . _)
               arg ...))
             #:with (_ ... (name . _)) (syntax-property this-syntax 'orig)))

  ;; convert type to string
  (define (type->string t)
    (define ->dat
      (syntax-parser
        [τ:base-type (syntax-e #'τ.name)]
        [τ:ctor-type (list* (syntax-e #'τ.name)
                            (map ->dat (syntax-e #'(τ.arg ...))))]
        [τ:bind-type (list* (syntax-e #'τ.name)
                            (map syntax-e (syntax-e #'(τ.bv ...)))
                            (map ->dat (syntax-e #'(τ.arg ...))))]
        [x (syntax->datum #'x)]))
    (~a (->dat t)))
  )


(begin-for-syntax
  ;; get the expected prototype for a syntax object
  (define (prototype-of stx)
    (or (syntax-property stx '~)
        ((current-type-eval) #'??)))

  ;; attach an expected prototype to a syntax object
  (define (attach-prototype stx p)
    (syntax-property stx '~ ((current-type-eval) p)))

  ;; are the given prototypes and full types compatible?
  (define (prototype-compat? p τ)
    (syntax-parse (list p ((current-type-eval) τ))
      [(~?? _) #t]
      [(b1:base-type b2:base-type)
       (free-identifier=? #'b1.internal-name #'b2.internal-name)]
      [((~→ p_i ...) (~→ τ_i ...))
       (andmap prototype-compat?
               (syntax-e #'(p_i ...))
               (syntax-e #'(τ_i ...)))]
      [(_ _) #f]))

  ;; expect the given type and prototype to be compatible
  (define (prototype-expect p τ #:src [src p])
    (unless (prototype-compat? p τ)
      (raise-syntax-error #f (format "expected type ~a, got ~a"
                                     (type->string p)
                                     (type->string τ))
                          src)))
  )

;; toplevel parser
(provide (rename-out [mod-beg #%module-begin]))
(define-typed-syntax mod-beg
  [(_ form ...) ≫
   [⊢ form ≫ form- ⇒ τ] ...
   #:with (τ/s ...) (map type->str (syntax-e #'(τ ...)))
   #:with test ((current-type-eval) #'(∀ (x) (→ x Int Int)))
   #:with test/s (type->string #'test)
   --------
   [≻ (#%module-begin
        (printf "~s : ~a\n"
                form-
                'τ/s)
        ...)]])

;; repl input
(provide (rename-out [repl #%top-interaction]))
(define-typed-syntax repl
  [(_ . e) ≫
   [⊢ e ≫ e- ⇒ τ]
   #:with τ/s (type->str #'τ)
   --------
   [≻ (printf "~s : ~a\n"
              e-
              'τ/s)]])

;; datum syntax
(provide (rename-out [dat #%datum]))
(define-typed-syntax dat
  [(_ . k:integer) ≫
   ; TODO: extract this 'idiom' out since it appears a lot
   #:do [(prototype-expect (prototype-of this-syntax)
                           #'Int
                           #:src #'k)]
   --------
   [⊢ (#%datum . k) ⇒ Int]]
  [(_ . k:str) ≫
   #:do [(prototype-expect (prototype-of this-syntax)
                           #'Str
                           #:src #'k)]
   --------
   [⊢ (#%datum . k) ⇒ Str]]
  [(_ . k) ≫
   --------
   [#:error "unsupported datum"]])

;; application
(provide (rename-out [app #%app]))
(define-typed-syntax app
  [(_ fun arg ...) ≫
   [⊢ fun ≫ fun- ⇒ (~→ arg_τ ... ret_τ)]
   #:do [(unless (= (stx-length #'(arg ...))
                    (stx-length #'(arg_τ ...)))
           (raise-syntax-error #f "wrong number of arguments to function"
                               #'fun))]

   #:with (arg* ...) (map attach-prototype
                          (syntax-e #'(arg ...))
                          (syntax-e #'(arg_τ ...)))
   [⊢ arg* ≫ arg- ⇒ arg_τ-] ...
   --------
   [⊢ (#%app fun- arg- ...) ⇒ ret_τ]]

  [(_ fun arg ...) ≫
   [⊢ fun ≫ fun- ⇒ τ]
   --------
   [#:error (format "type cannot be applied: ~a"
                    (type->string #'τ))]])


;; primitive ops
(define-syntax provide-typed
  (syntax-parser
    [(_ x:id τ)
     #'(provide-typed [x x] τ)]
    [(_ x:id (~datum :) τ)
     #'(provide-typed [x x] τ)]
    [(_ [x:id x-out:id] (~datum :) τ)
     #'(provide-typed [x x-out] τ)]
    [(_ [x:id x-out:id] τ:type)
     #:with x-internal (generate-temporary #'x)
     #'(begin
         (define-typed-syntax x-internal
           [_:id ≫
            #:do [(prototype-expect (prototype-of this-syntax)
                                    #'τ
                                    #:src this-syntax)]
            --------
            [⊢ x-out ⇒ τ]]
           [(a (... ...)) ≫
            --------
            [≻ (app a (... ...))]])
         (provide (rename-out [x-internal x-out])))]))

(provide-typed + : (→ Int Int Int))
(provide-typed add1 : (→ Int Int))
(provide-typed add2 : (→ Int Int))
(provide-typed nat-rec-int : (→ Int (→ Int Int) Int Int))

(define (add2 x) (+ 2 x))
(define (nat-rec-int b f n)
  (if (zero? n)
      b
      (f (nat-rec-int b f (sub1 n)))))
