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



;;; PROTOTYPES ;;;
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

  ;; is the prototype a full type (contains no ?? holes)
  (define prototype-full?
    (syntax-parser
      [~?? #f]
      [p:ctor-type (andmap prototype-full?
                           (syntax-e #'(p.arg ...)))]
      [p:bind-type (andmap prototype-full?
                           (syntax-e #'(p.arg ...)))]
      [_ #t]))

  ;; expect the given type and prototype to be compatible
  (define (prototype-expect p τ #:src [src p])
    (unless (prototype-compat? p τ)
      (raise-syntax-error #f (format "expected type ~a, got ~a"
                                     (type->string p)
                                     (type->string τ))
                          src)))

  ;; Coerce the prototype to a function with given arguments
  ;; Returns a list, where the first element is the list of
  ;;   argument >types<, and the second element is the return >prototype<.
  ;; Raises an error if the prototype cannot be coerced completely (if
  ;;   any ?? holes cannot be fulfilled).
  (define (prototype-function-coerce proto args
                                     #:src [src #f])
    (define (error-missing-info)
      (raise-syntax-error #f "some argument types are missing information and cannot be inferred" src))

    (syntax-parse proto
      [~?? (list (map (syntax-parser
                        [#f (error-missing-info)]
                        [τ #'τ])
                      (syntax-e args))
                 #'??)]

      [(~→ arg_p ... ret_p)
       (unless (= (stx-length #'(arg_p ...))
                  (stx-length args))
         (raise-syntax-error #f "wrong number of arguments expected to function" src))
       (let ([arg-ts (map (lambda (p t)
                            (cond
                              [(not (syntax-e t))
                               (if (prototype-full? p)
                                   p
                                   (error-missing-info))]
                              [(prototype-compat? p t) t]
                              [else
                               (raise-syntax-error #f (format "argument has wrong type; expected ~a, got ~a"
                                                              (type->string p)
                                                              (type->string t))
                                                   src)]))
                          (syntax-e #'(arg_p ...))
                          (syntax-e args))])
         (list arg-ts #'ret_p))]

      [_ (raise-syntax-error #f (format "unexpected function; expected ~a"
                                        (type->string proto))
                             src)]))

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

;; abstraction
(provide (rename-out [lam lambda]))

(begin-for-syntax
  (define-syntax-class id-maybe-ann
    #:attributes (id given-τ)
    (pattern id:id #:with given-τ #f)
    (pattern [id:id (~datum :) given-τ:type])))

(define-typed-syntax lam
  [(_ (arg:id-maybe-ann ...) e) ≫
   #:with (x ...) #'(arg.id ...)
   #:with ((x_τ ...) e_p) (prototype-function-coerce (prototype-of this-syntax)
                                                     #'(arg.given-τ ...)
                                                     #:src this-syntax)
   #:with e* (attach-prototype #'e #'e_p)
   [[x ≫ x- : x_τ] ... ⊢ e* ≫ e- ⇒ e_τ]
   --------
   [⊢ (lambda (x- ...) e-) ⇒ (→ x_τ ... e_τ)]])


;; primitive ops
(define-syntax provide-typed
  (syntax-parser
    [(_ x:id (~datum :) τ)             #'(provide-typed [x x] τ)]
    [(_ x:id τ)                        #'(provide-typed [x x] τ)]
    [(_ [x:id x-out:id] (~datum :) τ)  #'(provide-typed [x x-out] τ)]
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
           [(a (... ...)) ≫ ; treat macro as application when in head position
            --------
            [≻ (app a (... ...))]])
         (provide (rename-out [x-internal x-out])))]))

(provide-typed + : (→ Int Int Int))
(provide-typed add1 : (→ Int Int))
(provide-typed nat-rec-int : (→ Int (→ Int Int) Int Int))

(define (nat-rec-int b f n)
  (if (zero? n)
      b
      (f (nat-rec-int b f (sub1 n)))))
