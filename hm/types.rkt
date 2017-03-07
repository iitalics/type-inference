#lang racket
(provide (all-defined-out))

(define (types->strings tys)
  (define tv-hash (make-hasheq))
  (define new-var-name
    (let ([i 0])
      (lambda ()
        (set! i (add1 i))
        (substring "abcdefghijklmnopqrstuvwxyz"
                   (sub1 i) i))))
  (define (to-str t)
    (match t
      [(type-var x)
       (format "'~a" (hash-ref! tv-hash x new-var-name))]
      [(type-fun args ret)
       (format "(-> ~a)"
               (string-join (map to-str
                                 (append args (list ret)))))]
      [(type-basic id)
       (format "~a" (syntax-e id))]
      ))
  (map to-str tys))

(define (type->string t)
  (first (types->strings (list t))))

(define (type-write-proc t port mode)
  (case mode
    [(#f #t) (write-string (type->string t) port)]
    [else (write-string (cond
                          [(type-var? t) "#<type-var>"]
                          [(type-fun? t) "#<type-fun>"]
                          [(type-basic t) "#<type-basic>"]
                          ))]))

(struct type ()
  #:methods gen:custom-write [(define write-proc type-write-proc)])

(struct type-basic type (id))
(struct type-fun type (args ret))
(struct type-var type (ref))

(define (new-type)
  (type-var (gensym)))

(define (type-fun* . r)
  (match r
    [(list args ... ret) (type-fun args ret)]))



(define subs-hash (make-parameter (make-hasheq)))

(define-syntax-rule (with-new-subs body ...)
  (parameterize ([subs-hash (make-hasheq)])
    body ...))

(define (subs t)
  (match t
    [(type-var x)
     (match (hash-ref (subs-hash) x #f)
       [#f t]
       [u (subs u)])]
    [(type-fun args ret)
     (type-fun (map subs args) (subs ret))]
    [_ t]))

(define (subs:= x t)
  (hash-set! (subs-hash) x t))

(define (unify! t1 t2 stx)
  (define (u! t1 t2)
    (match* ((subs t1) (subs t2))
      [((type-var x) (type-var x))
       (void)]

      [((type-var x) t) (subs:= x t)]
      [(t (type-var y)) (subs:= y t)]

      [((type-basic b1) (type-basic b2))
       #:when (free-identifier=? b1 b2)
       (void)]

      [((type-fun args1 ret1) (type-fun args2 ret2))
       #:when (= (length args1)
                 (length args2))
       (for-each u! args1 args2)
       (u! ret1 ret2)]

      [(u1 u2)
       (match (types->strings (list u1 u2))
         [(list s1 s2)
          (raise-syntax-error
           #f (format "incompatible types: ~a, ~a\n" s1 s2)
           stx)])]))
  (u! t1 t2))
