#lang turnstile
(require (only-in racket/format ~a)
         (only-in racket/string string-join)
         racket/match)

(provide (type-out Int Str → all))

(define-base-types Int Str)

(define-type-constructor →
  #:arity >= 1)

(define-binding-type all
  #:bvs >= 1
  #:arity = 1)

(define-base-type ??)



(provide (rename-out [mod-beg #%module-begin]))
(define-syntax mod-beg
  (syntax-parser
    [(_ forms ...)
     #'(#%module-begin
        ; TODO: stuff here
        (void))]))

(define (type->str t)
  (let ([orig (match (syntax-property t 'orig)
                [(list _ ... o) o]
                [_ t])])
    (match (syntax->datum orig)
      [(list ts ...) (string-join (map type->str ts)
                                  #:before-first "("
                                  #:after-last ")")]
      [x (~a x)])))

(provide (rename-out [top-repl #%top-interaction]))
(define-syntax top-repl
  (syntax-parser
    [(_ . e)
     #:with (_ _ (e-) (τ)) (infer (list #'e))
     #:with τ/s (type->str #'τ)
     #'(printf "~s\n - ~a\n"
               e-
               'τ/s)]))

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
  [(_ f e_i ...) ≫
   [⊢ f ≫ f- ⇒ τ_f]
   #:with (~→ τ_i ... τ_r) #'τ_f
   #:fail-unless (stx-length=? #'(τ_i ...) #'(e_i ...))
   (format "incorrect # of arguments, expected ~a"
           (stx-length #'(τ_i ...)))
   [⊢ e_i ≫ e_i- ⇐ τ_i] ...
   --------
   [⊢ (#%app- f- e_i- ...) ⇒ τ_r]])

(provide (rename-out [lam lambda]))
(define-typed-syntax lam #:datum-literals (:)
  [(_ ([x_i:id : τ_i:type] ...) e) ≫
   [[x_i ≫ x_i- ⇒ τ_i.norm] ... ⊢ e ≫ e- ⇒ τ_r]
   --------
   [⊢ (lambda- (x_i- ...) e-) ⇒ (→ τ_i.norm ... τ_r)]])


(provide (rename-out [t+ +]))
(define-primop t+
  + : (→ Int Int Int))
