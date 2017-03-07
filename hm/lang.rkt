#lang racket

(require (for-syntax syntax/parse
                     racket/syntax
                     "types.rkt"
                     "infer.rkt")
         racket/require-syntax)


(provide (rename-out [mod-begin #%module-begin]))
(define-syntax (mod-begin stx)
  (syntax-parse stx
    #:literals (req-typed-in)
    [(_ (req-typed-in . req-spec) ...
        forms ...)
     (with-syntax
       ([(out ...)
         (for/list ([e (in-list (syntax-e #'(forms ...)))])
           (with-new-subs
             (let*-values ([(e- e.t) (infer e)])
               #`(printf "~s => ~s\n- : ~a\n\n"
                         '#,e
                         '#,e-
                         '#,(subs e.t)))))])
       #'(#%module-begin
          (req-typed-in . req-spec) ...
          out ...))]))

(provide (rename-out [top-interaction #%top-interaction]))
(define-syntax (top-interaction stx)
  (syntax-parse stx
    [(_ . e)
     (with-new-subs
       (let-values ([(e- e.t) (infer #'e)])
         #`(printf "~s\n- : ~a\n"
                   #,e-
                   '#,(subs e.t))))]))

(provide (rename-out [datum #%datum]))
(define-syntax datum
  (syntax-parser-bidirectional
   [(_ . d:integer)
    #:infer (values #'(#%datum . d) (type-basic #'int))]

   [(_ . d:str)
    #:infer (values #'(#%datum . d) (type-basic #'str))]

   [(_ . d)
    (raise-syntax-error #f "unsupported datum" #'d)]
   ))

(provide (rename-out [lam lambda]))
(define-syntax lam
  (syntax-parser-bidirectional
   [(_ (xs:id ...) e:expr)
    #:infer (let ([xs- (generate-temporaries #'(xs ...))]
                  [xs.t (map (lambda _ (new-type)) (syntax-e #'(xs ...)))])
              (let-values ([(e- e.t) (infer #'e
                                            (map list
                                                 (syntax-e #'(xs ...))
                                                 xs.t
                                                 xs-))])
                (values #`(lambda #,xs- #,e-)
                        (type-fun xs.t e.t))))
    ]))

(provide (rename-out [app #%app]))
(define-syntax app
  (syntax-parser-bidirectional
   [(_ fn:expr args:expr ...)
    #:infer (let-values
                ([(fn- fn.t) (infer #'fn)]
                 [(args- args.t)
                  ; ugly way to collect all types
                  (for/fold ([args- '()] [args.t '()])
                            ([arg (in-list (reverse (syntax-e #'(args ...))))])
                    (let-values ([(arg- arg.t) (infer arg)])
                      (values (cons arg- args-)
                              (cons arg.t args.t))))]
                 [(ret.t)
                  ; this is kinda bad; should look at the actual type of fn.t
                  ; and actually do >>bidirectional<< inference, but this works
                  ; for all correct programs
                  (new-type)])
              ; imperative unify!
              (unify! fn.t (type-fun args.t ret.t) #'fn)
              (values #`(#%app #,fn- . #,args-)
                      ret.t))
    ]))


(define-for-syntax (parse-type stx)
  (syntax-parse stx
    #:literals (quote)
    #:datum-literals (->)
    [(-> args:expr ... ret:expr)
     (type-fun (map parse-type
                    (syntax-e #'(args ...)))
               (parse-type #'ret))]
    [(quote v)
     (raise-syntax-error #f "type variable syntax unsupported right now" stx)]
    [d:id
     (type-basic #'d)]))

(provide (rename-out [req-typed-in require-typed]))
(define-syntax req-typed-in
  (syntax-parser
    [(_ mod [symbol:id type:expr] ...)
     (with-syntax
       ([( (new-symbol mac-def) ...)
         (for/list ([sym (syntax-e #'(symbol ...))]
                    [type-stx (syntax-e #'(type ...))])
           (let* ([sym- (generate-temporary #'sym)]
                  [type (parse-type type-stx)]
                  [tvt (make-typed-var-transformer type sym-)])
             (list sym-
                   #`(define-syntax #,sym #,tvt))))])
       #'(begin
           (require (only-in mod
                             [symbol new-symbol] ...))
           (printf "; import `~a' => `~a'\n\n"
                   'symbol 'new-symbol) ...
           mac-def ...))]))


(provide #%top)
