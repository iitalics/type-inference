#lang turnstile
(require (only-in racket/format ~a)
         (only-in racket/string string-join)
         syntax/parse
         (only-in racket/sequence
                  in-syntax))

(provide (type-out Int Str →))

(define-type-constructor → #:arity >= 1)
(define-binding-type ∀ #:arity = 1 #:bvs >= 1)
(define-base-types Int Str)
(define-base-type ??)

(begin-for-syntax
  ;;;; type utilities ;;;;

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

  ;;;; syntax parse utilities ;;;;

  (define-syntax-class id-maybe-ann
    #:attributes (id given-τ)
    (pattern id:id
             #:with given-τ #f)
    (pattern [id:id (~datum :) τ:type]
             #:with given-τ ((current-type-eval)
                             (datum->syntax #'_ (syntax->datum #'τ)))))

  (define-syntax-class is-curly
    (pattern _ #:when (char=? (syntax-property this-syntax 'paren-shape)
                              #\{)))

  (define-syntax ~curly
    (pattern-expander
     (syntax-parser
       [(_ pats ...)
        #'(~and _:is-curly
                (pats ...))]
       [_:id
        #'_:is-curly])))
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
       (and (stx-length=? #'(p_i ...) #'(τ_i ...))
            (andmap prototype-compat?
                    (syntax-e #'(p_i ...))
                    (syntax-e #'(τ_i ...))))]
      [(_ _)
       (printf "weird types in compat? :\n  ~s\n  ~s\n"
               (syntax->datum p)
               (syntax->datum τ))
       #f]))

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
      (raise-syntax-error #f "missing information; cannot infer function type" src))

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
                                                              (syntax->datum p)
                                                              (syntax->datum t))
                                                   src)]))
                          (syntax-e #'(arg_p ...))
                          (syntax-e args))])
         (list arg-ts #'ret_p))]

      [_ (raise-syntax-error #f (format "unexpected function; expected ~a"
                                        (type->string proto))
                             src)]))

  ;; Remove the given variables from a type, resulting in a prototype
  (define (remove-vars vars τ
                       #:cmp [cmp bound-identifier=?])
    (substs (map (lambda (_) ((current-type-eval) #'??)) vars)
            vars
            τ
            cmp))

  ;; unification algorithm
  (define (unify init-τ1 init-τ2 cstrs
                 #:cmp cmp
                 #:vars vars
                 #:src src)
    (define (error-incompat cstrs)
      (let ([Xs (map car cstrs)]
            [τs (map cdr cstrs)])
        (raise-syntax-error #f
                            (format "incompatible types: ~a and ~a"
                                    (type->string (substs τs Xs init-τ1))
                                    (type->string (substs τs Xs init-τ2)))
                            src)))

    (define (sub0 e cstrs)
      (syntax-parse e
        [x:id
         (let ([τ (assoc #'x cstrs cmp)])
           (if τ (sub0 (cdr τ) cstrs) e))]
        [_ e]))

    (define (occurs? x τ)
      (syntax-parse τ
        [y:id (cmp x #'y)]
        [(a . b) (or (occurs? x #'a)
                     (occurs? x #'b))]
        [() #f]))

    (define (Ω τ1 τ2 cstrs)
      ;(printf "Ω(~a, ~a)\n" (type->string τ1) (type->string τ2))
      (syntax-parse (list (sub0 τ1 cstrs)
                          (sub0 τ2 cstrs))
        [(x:id y:id) #:when (and (member #'x vars cmp)
                                 (cmp #'x #'y))
         cstrs]

        [(~or (x:id τ)
              (τ x:id))
         #:when (member #'x vars cmp)
         (when (occurs? #'x #'τ)
           (raise-syntax-error #f
                               (format "occurs check failed; infinite type ~a ~~ ~a"
                                       (type->string #'x)
                                       (type->string #'τ))
                               src))
         (cons (cons #'x #'τ) cstrs)]

        [(a:id b:id) #:when (free-identifier=? #'a #'b)
         cstrs]

        [((a . b) (c . d))
         (Ω #'b #'d (Ω #'a #'c cstrs))]

        [(() ()) cstrs]
        [(t u) (error-incompat cstrs)]))
    (Ω init-τ1 init-τ2 cstrs))
  )








;;;; toplevel parser ;;;;
(provide (rename-out [mod-beg #%module-begin]))
(define-typed-syntax mod-beg
  [(_ form ...) ≫
   [⊢ form ≫ form- ⇒ τ] ...
   #:with (τ/s ...) (map type->string (syntax-e #'(τ ...)))
   --------
   [≻ (#%module-begin
       (printf "~s : ~a\n"
               form-
               'τ/s)
       ...)]])



;;;; repl input ;;;;
(provide (rename-out [repl #%top-interaction]))
(define-typed-syntax repl
  [(_ . e) ≫
   [⊢ e ≫ e- ⇒ τ]
   #:with τ/s (type->string #'τ)
   --------
   [≻ (printf "~s : ~a\n"
              e-
              'τ/s)]])



;;;; datum syntax ;;;;
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





;;;; application ;;;;
(provide (rename-out [app #%app]))
(define-typed-syntax app

  ;;
  ;; application, with generalization annotated ;;
  ;;
  [(_ fun (~curly ~! X_τ ...) arg ...) ≫
   [⊢ fun ≫ fun- ⇒ (~∀ (X ...) (~→ arg_τ ... ret_τ))]
   ; check argument lengths
   #:fail-unless (stx-length=? #'(X_τ ...)
                               #'(X ...))
   "wrong number of arguments to instantiation"
   #:fail-unless (stx-length=? #'(arg ...)
                               #'(arg_τ ...))
   "wrong number of arguments to function"

   ; eval types and substitute
   #:with (X_τ- ...) (map (current-type-eval)
                          (syntax-e #'(X_τ ...)))
   #:with (arg_τ- ... ret_τ-) (map (curry substs
                                          (syntax-e #'(X_τ- ...))
                                          (syntax-e #'(X ...)))
                                   (syntax->list #'(arg_τ ... ret_τ)))

   ; typecheck arguments
   #:with (arg* ...) (map attach-prototype
                          (syntax-e #'(arg ...))
                          (syntax-e #'(arg_τ- ...)))
   [⊢ arg* ≫ arg- ⇒ _] ...
   #:do [(prototype-expect (prototype-of this-syntax)
                           #'ret_τ-
                           #:src this-syntax)]
   --------
   [⊢ (#%app fun- arg- ...) ⇒ ret_τ-]]


  ;;
  ;; plain application, without generalization ;;
  ;;
  [(_ fun arg ...) ≫
   [⊢ fun ≫ fun- ⇒ (~→ arg_τ ... ret_τ)]
   #:fail-unless (stx-length=? #'(arg ...)
                               #'(arg_τ ...))
   "wrong number of arguments to function"
   ; infer arguments
   #:with (arg* ...) (map attach-prototype
                          (syntax-e #'(arg ...))
                          (syntax-e #'(arg_τ ...)))
   [⊢ arg* ≫ arg- ⇒ _] ...
   #:do [(prototype-expect (prototype-of this-syntax)
                           #'ret_τ
                           #:src this-syntax)]
   --------
   [⊢ (#%app fun- arg- ...) ⇒ ret_τ]]


  ;;
  ;; application, generalization inferred ;;
  ;;
  [(_ fun arg ...) ≫
   [⊢ fun ≫ fun- ⇒ (~∀ (X ...) (~→ arg_τ ... ret_τ))]
   #:fail-unless (stx-length=? #'(arg ...)
                               #'(arg_τ ...))
   "wrong number of arguments to function"
   ; remove variables from arguments
   #:with (arg_p ... ret_p) (map (curry remove-vars
                                        (syntax-e #'(X ...)))
                                 (syntax->list #'(arg_τ ... ret_τ)))

   ; infer arguments
   #:with (arg* ...) (map attach-prototype
                          (syntax-e #'(arg ...))
                          (syntax-e #'(arg_p ...)))
   [⊢ arg* ≫ arg- ⇒ arg_τ*] ...

   ; unify to determine variables
   #:with ((Y . τ) ...)
   (foldl (lambda (τ1 τ2 src cstrs)
            (unify τ1 τ2 cstrs
                   #:cmp bound-identifier=?
                   #:vars (syntax-e #'(X ...))
                   #:src src))
          '()
          (syntax-e #'(arg_τ ...))
          (syntax-e #'(arg_τ* ...))
          (syntax-e #'(arg ...)))
   #:do [(unless (stx-length=? #'(Y ...) #'(X ...))
           (raise-syntax-error #f
                               "not all type variables were able to be inferred"
                               this-syntax))]

   ; substitute variables in return type
   #:with ret_τ- (substs #'(τ ...)
                         #'(Y ...)
                         #'ret_τ)

   #:do [(prototype-expect (prototype-of this-syntax)
                           #'ret_τ-
                           #:src this-syntax)]
   --------
   [⊢ (#%app fun- arg- ...) ⇒ ret_τ-]]


  ;;
  ;; invalid application ;;
  ;;
  [(_ fun arg ...) ≫
   [⊢ fun ≫ fun- ⇒ τ]
   --------
   [#:error (format "type cannot be applied: ~a"
                    (type->string #'τ))]])

;; abstraction
(provide (rename-out [lam lambda]))
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

;; explicit annotation
(provide (rename-out [annotate ann]))
(define-typed-syntax annotate
  [(_ e : τ) ≫ -------- [≻ (annotate e τ)]]
  [(_ e:expr p:type) ≫
   #:with e* (attach-prototype #'e #'p)
   [⊢ e* ≫ e- ⇒ τ]
   #:do [(prototype-expect (prototype-of this-syntax)
                           #'τ
                           #:src this-syntax)]
   --------
   [⊢ e- ⇒ τ]])


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
            [⊢ x ⇒ τ]]
           [(a (... ...)) ≫ ; treat macro as application when in head position
            --------
            [≻ (app a (... ...))]])
         (provide (rename-out [x-internal x-out])))]))

(provide-typed + : (→ Int Int Int))
(provide-typed add1 : (→ Int Int))
(provide-typed nat-rec : (∀ (α) (α (→ α α) Int . → . α)))
(provide-typed with-rand : (∀ (α) (→ Int (→ Int α) α)))

(define (nat-rec b f n)
  (if (zero? n)
      b
      (f (nat-rec b f (sub1 n)))))

(define (with-rand n f)
  (f (random n)))
