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
(define-base-types Int Str)
(define-base-type ??)


(begin-for-syntax
  ;; get the expected prototype for a syntax object
  (define (prototype-of stx)
    (or (syntax-property stx '~) #'??))

  ;; attach an expected prototype to a syntax object
  (define (attach-prototype stx p)
    (syntax-property stx '~ p))

  ;; are the given prototypes and full types compatible?
  (define (prototype-compat? p τ)
    (syntax-parse (list p τ)
      [((~datum ??) _) #t]
      [(x:id y:id) (free-identifier=? #'x #'y)]
      [((ps ...) (τs ...))
       (andmap prototype-compat?
               (syntax-e #'(ps ...))
               (syntax-e #'(τs ...)))]
      [(_ _) #f]))

  ;; expect the given type and prototype to be compatible
  (define (prototype-expect p τ #:src [src p])
    (unless (prototype-compat? p τ)
      (raise-syntax-error #f (format "expected type ~s, got ~s"
                                     (syntax-e #'p)
                                     (syntax-e #'τ))
                          src)))

  ;; convert type to string representation
  (define (type->str stx)
    (define (orig stx)
      (syntax-parse (match (syntax-property stx 'orig)
                      [#f stx]
                      [origs (last origs)])
        [(e ...) (map orig (syntax-e #'(e ...)))]
        [k (syntax-e #'k)]))
    (format "~a" (orig stx)))
  )

;; toplevel parser
(provide (rename-out [mod-beg #%module-begin]))
(define-typed-syntax mod-beg
  [(_ form ...) ≫
   [⊢ form ≫ form- ⇒ τ] ...
   #:with (τ/s ...) (map type->str (syntax-e #'(τ ...)))
   --------
   [≻ (#%module-begin
        (printf "~s : ~a\n\n"
                form-
                'τ/s) ...)]])

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

;; lambda syntax
(provide (rename-out [lam lambda]))
(define-typed-syntax lam
  #:datum-literals (:)
  [(_ ((~and arg (~or :id :type-bind)) ...) e)
   #:with (τ_exp ... p_ret)
