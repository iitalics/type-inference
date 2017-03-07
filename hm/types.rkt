#lang racket
(provide (all-defined-out))

(define (type-write-proc t port mode)
  (define simple?
    (or/c type-var?
          type-basic?))
  (match t
    [(type-var x)
     (fprintf port "~v" x)]
    [(type-fun (? simple? arg) ret)
     (fprintf port "~a -> ~a" arg ret)]
    [(type-fun arg ret)
     (fprintf port "(~a) -> ~a" arg ret)]
    [(type-basic id)
     (fprintf port "~a" (syntax-e id))]))

(struct type ()
  #:methods gen:custom-write [(define write-proc type-write-proc)])

(struct type-basic type (id))
(struct type-fun type (arg ret))
(struct type-var type (ref))

(define (new-type)
  (type-var (gensym)))



(define (unify! t1 t2)
  (printf "; unify: ~a      ~a\n"
          t1 t2))
