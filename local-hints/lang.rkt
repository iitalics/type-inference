#lang turnstile
(require (only-in racket/format ~a)
         (only-in racket/string string-join)
         (only-in racket/function identity const)
         racket/match
         syntax/parse
         (only-in racket/sequence
                  in-syntax))

(provide (type-out Int Str → ∀))

(define-base-types Int Str)

(define-type-constructor →
  #:arity >= 1)

(define-binding-type ∀
  #:bvs >= 1
  #:arity = 1)

(define-base-type ??)


;; mod-beg: toplevel parser
(provide (rename-out [mod-beg #%module-begin]))
(define-syntax mod-beg
  (syntax-parser
    [(_ forms ...)
     #'(#%module-begin
        ; TODO: stuff here
        (void))]))

;; top-repl: REPL handler
(provide (rename-out [top-repl #%top-interaction]))
(define-syntax top-repl
  (syntax-parser
    [(_ . e)
     #:with (_ _ (e-) (τ)) (infer (list #'e))
     #:with τ/s (type->str #'τ)
     #'(printf "~s\n - ~a\n"
               e-
               'τ/s)]))

;; convert type to string representation
(define (type->str t)
  (let ([orig (match (syntax-property t 'orig)
                [(list _ ... o) o]
                [_ t])])
    (match (syntax->datum orig)
      [(list ts ...) (string-join (map type->str ts)
                                  #:before-first "("
                                  #:after-last ")")]
      [x (~a x)])))

(begin-for-syntax
  ;; syntax class for checking that argument counts match
  (define-syntax-class arg#-check
    (pattern
     ([e ...] [τ ...])
     #:fail-unless (stx-length=? #'(e ...) #'(τ ...))
     (format "wrong number of arguments to function; expected ~a, got ~a"
             (stx-length #'(τ ...))
             (stx-length #'(e ...)))))

  ;; unification algorithm
  ;;; (unify-all (listof identifier?)
  ;;;            syntax?)
  ;;;   -> (listof type-stx?)
  (define (unify-all Xs cs)
    (with-syntax ([(X ...) Xs] [({τ1 τ2} ...) cs])
      (for/list ([X (in-syntax #'(X ...))])
        #'??)))
  )


(provide (rename-out [datum #%datum]))
(define-typed-syntax datum
  [(_ . k:integer) ≫
   --------
   [⊢ (#%datum- . k) ⇒ Int]]
  [(_ . k:str) ≫
   --------
   [⊢ (#%datum- . k) ⇒ Str]]
  [(_ . x) ≫
   --------
   [#:error (type-error #:src #'x
                        #:msg "unsupported datum: ~v" #'x)]])

(provide (rename-out [app #%app]))
(define-typed-syntax app
  ; normal application
  [(_ f e_i ...) ≫
   [⊢ f ≫ f- ⇒ (~→ τ_i ... τ_r)]
   #:with :arg#-check #'([e_i ...] [τ_i ...])
   [⊢ e_i ≫ e_i- ⇐ τ_i] ...
   --------
   [⊢ (#%app- f- e_i- ...) ⇒ τ_r]]

  ; ∀ inference
  [(_ f e_i ...) ≫
   [⊢ f ≫ f- ⇒ (~∀ (X ...) (~→ τ_i ... τ_r))]
   #:with :arg#-check #'([e_i ...] [τ_i ...])
   [⊢ e_i ≫ e_i- ⇒ τ_j] ...
   #:with (σ ...) (unify-all #'(X ...)
                             #'({τ_i τ_j} ...))
   #:with τ_r- (substs #'(σ ...)
                       #'(X ...)
                       #'τ_r)
   --------
   [⊢ (#%app- f- e_i- ...) ⇒ τ_r-]]

  ; ??
  [(_ f e_i ...) ≫
   [⊢ f ≫ _ ⇒ τ_f]
   --------
   [#:error (type-error #:src #'f
                        #:msg "cannot apply type: ~a" #'τ_f)]]
  )

(provide (rename-out [lam lambda]))
(define-typed-syntax lam #:datum-literals (:)
  [(_ ([x_i:id : τ_i:type] ...) e) ≫
   [[x_i ≫ x_i- : τ_i.norm] ... ⊢ e ≫ e- ⇒ τ_r]
   --------
   [⊢ (lambda- (x_i- ...) e-) ⇒ (→ τ_i.norm ... τ_r)]])


(provide
 (typed-out [+ : (→ Int Int Int)]
            [[identity : (∀ (X) (→ X X))] id]
            [const : (∀ (X) (→ X (∀ (Y) (→ Y X))))]
            ))
