#lang racket
(require syntax/parse
         syntax/transformer
         racket/syntax
         "types.rkt"
         (for-template racket/base))

;; This synax is returned by successful inference/checking, with appropriate
;; syntax properties attached to it. It's kind of hacky ATM; I originally
;; had the untyped syntax be return by the inference macros, with only
;; type information as a syntax property, but local-expand would end up
;; expanding it prematurely, which ended up with really crazy scoping set
;; issues.
(define (make-output-token tg)
  (syntax-property #`(quote #,(generate-temporary #'OUTPUT-TOKEN)) tg #t))
(define tag:output-infer 'OUT-INF)
(define tag:output-check 'OUT-CHK)

;; `tag:dir' is applied in (infer ...)/(check ...) to tell the macro down the line
;; which kind of inference is going on
(define tag:dir 'DIRECTION)

;; `tag:typeof' is used during (check ...) to tell the macro what type it is expected,
;; and is used by (bidirectional ...) to move type information up to parent syntax
(define tag:typeof 'TYPEOF)

;; `tag:untyped' is used by (bidirectional ...) to tell parent syntax what the untyped
;; result of this macro should be
(define tag:untyped 'UNTYPED)


;;;; invoking type judgements ;;;;

;; Infer the type of the given expression, possibly introducing new
;; context. Returns two values: the resulting untyped syntax, and
;; the inferred type.
(provide infer)
(define (infer stx [ctx '()])
  (let* ([stx+tag (stx-props stx
                             tag:dir 'infer)]
         [stx+ctx (introduce-context ctx stx+tag)]
         [out (expand/trim stx+ctx tag:output-infer)])
    (values (syntax-property out tag:untyped)
            (syntax-property out tag:typeof))))

;; Convenience to apply context variables as rest arguments
(provide infer*)
(define (infer* stx . ctx) (infer stx ctx))

;; Check that the given expression is of the expected type, possible
;; introducing new context. Returns the resulting untyped syntax when
;; successful.
(provide check)
(define (check ty stx [ctx '()])
  (let* ([stx+tag (stx-props stx
                             tag:dir 'check
                             tag:typeof ty)]
         [stx+ctx (introduce-context ctx stx+tag)]
         [out (expand/trim stx+ctx tag:output-check)])
    (syntax-property out tag:untyped)))

;; Convenience to apply context variables as rest arguments
(provide check*)
(define (check* ty stx . ctx) (check ty stx ctx))


;; Respond to requests for type inference on the given syntax with
;; the given thunks.
;; #:infer specifies a zero-argument function that should return two
;; values: the resulting untyped syntax, and the inferred type.
;; #:check specifies a one-argument function that accepts the expected
;; type and returns the resulting untyped syntax.
(provide bidirectional)
(define (bidirectional stx
                       #:infer inf-fn
                       #:check [chk-fn (lambda _
                                         (error "no typechecking"))])
  (match (syntax-property stx tag:dir)
    ['infer
     (let-values ([(stx- stx.t) (inf-fn)])
       (stx-props (make-output-token tag:output-infer)
                  tag:typeof stx.t
                  tag:untyped stx-))]

    ['check
     (let ([stx- (chk-fn (syntax-property stx tag:typeof))])
       (stx-props (make-output-token tag:output-check)
                  tag:untyped stx-))]

    [_ (printf "warning: (bidirectional) recieved no action...\n")
       (raise-syntax-error #f "bidirectional" stx)]))


(provide syntax-parser-bidirectional)
(require (for-syntax syntax/parse))
(define-syntax (syntax-parser-bidirectional stx)
  (define (parse-bidir-args parts)
    (syntax-parse parts
      ;; #:infer <expr>
      [(#:infer e:expr . next)
       (with-syntax ([next-args (parse-bidir-args #'next)])
         #'(#:infer (lambda () e) . next-args))]
      ;; #:check <var> <expr>
      [(#:check x:id e:expr . next)
       (with-syntax ([next-args (parse-bidir-args #'next)])
         #'(#:check (lambda (x) e) . next-args))]
      [(arg . next)
       (with-syntax ([next-args (parse-bidir-args #'next)])
         #'(arg . next-args))]
      [() #'()]))
  (syntax-parse stx
    [(_ [patn . parts] ...)
     (with-syntax ([(bidir-args ...)
                    (map parse-bidir-args
                         (syntax->list #'(parts ...)))])
       #'(lambda (sstx)
           (syntax-parse sstx
             [patn (bidirectional sstx . bidir-args)] ...)))]))





;;; util ;;;
;; Convenience function for chaining (syntax-property ...) calls
(define (stx-props stx . args)
  (let trav ([stx stx] [args args])
    (match args
      [(list* k v args-)
       (trav (syntax-property stx k v)
             args-)]
      ['() stx]
      [_ (error "odd # of arguments to stx-props")])))

;; Expand and then trim the given syntax, expected the given tag
;; in the resultant output token
(define (expand/trim stx tg)
  (trim-syntax (local-expand stx
                             'expression
                             '())
               tg))

;; Trim excess syntax artifacts (e.g. (let-values () ...)) from the
;; given syntax, and return the resulting output token, OR error if
;; that token does not have the correct tag.
(define (trim-syntax stx tg)
  (syntax-parse stx
    #:literals (let-values #%expression)
    ; remove meaningless syntax left over by macros
    [(let-values () stx-)
     (trim-syntax #'stx- tg)]
    [(#%expression stx-)
     (trim-syntax #'stx- tg)]

    [_ (cond
         ; if it contains the expected tag, we are finished
         [(syntax-property stx tg) stx]

         ; this is the base value that should contain type information
         ; this happens when we encounter untyped symbols/syntax
         [else (displayln (syntax-property-symbol-keys stx))
               (raise-syntax-error #f "missing type information" stx)])]))

;; Introduce variables into the context of the given syntax.
;; The context should be a list of three-element lists: the variable name,
;; it's type, and the untyped syntax it expands into.
(define (introduce-context ctx stx)
  ; should this be foldr??
  (foldl (lambda (binding next)
           (match-let ([(list x x.t x-) binding])
             (let ([tvt (make-typed-var-transformer x.t x-)])
               #`(let-syntax ([#,x #,tvt])
                   #,next))))
         stx
         ctx))

;; Create a rename-transformer with type information and untyped expansion.
(provide make-typed-var-transformer)
(define (make-typed-var-transformer type expands-to)
  (make-set!-transformer
   (lambda (stx)
     (syntax-parse stx
       ; TODO: make this more modular?
       [_:id (bidirectional stx #:infer (lambda () (values expands-to type)))]
       [(a . b)
        ; manually build a new #%app to prevent this from being treated as a non-identifier
        (datum->syntax stx
                       (list* '#%app
                              #'a
                              #'b))]))))
