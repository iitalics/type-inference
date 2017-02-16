#lang racket
(provide (all-defined-out))

;;;; AST ;;;;
(struct ast (src-pos))

(define src-pos?
  (list/c string?    ; file name
          integer?   ; line
          integer?)) ; col

;;; raise error with backtrace
(define (fail sp . args)
  (error
   (format "error: ~a~a"
           (apply format args)
           (string-append*
            (map (lambda (src-pos)
                   (match-let ([(list fn ln col) src-pos])
                     (format "\n  in ~a:~a:~a" fn ln col)))
                 (fail-pos))))))

;;; backtrace
(define fail-pos (make-parameter '()))

;;; add to backtrace
(define (with-fail-pos src-pos f)
  (match src-pos
    [#f (f)]
    [(list _ _ _)
     (parameterize ([fail-pos (cons src-pos (fail-pos))])
       (f))]
    [_ (error 'error "invalid src-pos: ~v" src-pos)]))



;;;; Expression Syntax ;;;;
(struct e-var    ast (id) #:transparent)
(struct e-app    ast (fun arg) #:transparent)
(struct e-tuple  ast (elems) #:transparent)
(struct e-case   ast (kern cases) #:transparent)
(struct e-fun    ast (arg body) #:transparent)
(struct e-let    ast (arg rhs body) #:transparent)
(struct e-object ast (vars methods) #:transparent)
(struct e-method ast (noun verb) #:transparent)

(define (e-app* sp fn . l)
  (match l
    [(list arg) (e-app sp fn arg)]
    [(list* args) (e-app sp (e-tuple sp args))]))

(define literal?
  (or/c number? string? boolean?))

(define expr?
  (or/c literal?
        e-var? e-app?
        e-tuple? e-case?
        e-fun? e-let?
        e-object? e-method?))



;;;; Pattern Syntax ;;;;
(struct p-var   ast (id) #:transparent)
(struct p-tuple ast (elems) #:transparent)
(struct p-app   ast (ctor args) #:transparent)

(define patn?
  (or/c literal?
        p-var? p-tuple? p-app?))



;;;; Type Syntax ;;;;
(struct t-fun    (arg ret) #:transparent)
(struct t-tuple  (elems) #:transparent)
(struct t-data   (def args) #:transparent)
(struct t-object (methods) #:transparent)
(struct forall   (var type) #:transparent)

(define (t-fun* . l)
  (match l
    [(list arg ret) (t-fun arg ret)]
    [(list args ... ret) (t-fun (t-tuple args) ret)]))

(define type?
  (or/c t-fun? t-tuple?
        t-data? t-object?
        symbol?))

(define quantified?
  (or/c forall?
        type?))

(define ctx?
  (listof (cons/c symbol? quantified?)))

(define object-type-method?
  (cons/c symbol? type?))



;;;; Definitions ;;;;
(struct typing-def     ast (type) #:transparent)
(struct fun-def        ast (arg body) #:transparent)
(struct data-def       ast (ctors) #:transparent)
(struct type-alias-def ast (type) #:transparent)
(struct obj-var-def    ast (name initial) #:transparent)
(struct obj-method-def ast (name arg body) #:transparent)

(define ctor?
  (cons/c symbol?
          (listof type?)))

(define toplevel-def?
  (or/c typing-def?
        fun-def?
        data-def?
        type-alias-def?))

(define toplevel?
  (listof (cons/c symbol? toplevel-def?)))
