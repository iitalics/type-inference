#lang racket
(require "ast.rkt")
(require (only-in rackunit
                  check-equal?
                  check-exn
                  test-case))
(provide val?
         env?
         default-env
         ;
         evaluate
         pattern-match)


;;;; Evalulation ;;;;

(define val-fun? procedure?)
(define val-tuple? list?)
(define val-data? vector?)
(define val-object? hash?)
(define val?
  (or/c literal?
        val-fun?
        val-tuple?
        val-data?
        val-object?))

(define env?
  (listof (cons/c symbol? val?)))

(define default-env
  (make-parameter
   (list (cons '+ (lambda (v)
                    (+ (first v)
                       (second v))))
         )))

(define (evaluate e [env (default-env)])
  (match e
    [(? literal? l) l]
    ; variable
    [(or (e-var _ x)
         (? symbol? x))
     (cdr (assoc x env symbol=?))]
    ; tuple
    [(e-tuple _ (list e))
     (evaluate e env)]
    [(e-tuple _ elems)
     (map (lambda (el) (evaluate el env)) elems)]

    ; application
    [(e-app _ fun arg)
     (apply (evaluate fun env)
            (list (evaluate arg env)))]

    ; case e | p -> e ...
    [(e-case src e cases)
     (let ([v (evaluate e env)])
       (let find-case ([cases cases])
         (cond
           [(pair? cases)
            (let ([new-env (pattern-match (caar cases) env)])
              (if new-env
                  (evaluate (cdar cases) new-env)
                  (find-case (cdr cases))))]
           [else
            (with-fail-pos src
              (fail "no matching case"))])))]

    ; fn (p) ...
    [(e-fun src arg body)
     (lambda (val)
       (call-with-arg src arg val body env))]

    ; let x = e in e
    [(e-let src x rhs body)
     (evaluate body
               (cons (cons x (evaluate rhs env)) env))]

    ; object { ... }
    [(e-object src vars methods)
     (let ([new-env
            (append (map (lambda (var)
                           (match-let ([(obj-var-def _ name e) var])
                             (cons name (evaluate e env))))
                         vars)
                    env)]
           [obj (make-hash)])
       (for ([method (in-list methods)])
         (match-let ([(obj-method-def _ name arg body) method])
           (hash-set! obj name
                      (lambda (val self)
                        (call-with-arg src arg val body
                                       (cons (cons 'this self) env))))))
       obj)]

    ; o#func
    [(e-method src noun verb)
     (let* ([obj (evaluate noun env)]
            [err (lambda ()
                   (with-fail-pos src
                     (fail "no method `~a' in object" verb)))]
            [fn (hash-ref obj verb err)])
       (lambda (v)
         (fn v obj)))]
    ))

;;; refactored some common code of function calls
(define (call-with-arg src arg val body env)
  (with-fail-pos src
    (thunk
     (let ([new-env (pattern-match arg val env)])
       (if new-env
           (evaluate body new-env)
           (fail "arguments do not match argument pattern"))))))



;;; match value against pattern. returns updated env if success,
;;; returns #f if does not match pattern.
(define (pattern-match pat val [env '()])
  (match pat
    [(? literal? l)
     (and (equal? l val) env)]
    ; var
    [(p-var _ x)
     (cons (cons x val) env)]
    ; (tuple, ..)
    [(p-tuple _ elem-pats)
     (and (val-tuple? val)
          (= (length elem-pats)
             (length val))
          (foldl (lambda (p v e)
                   (and e (pattern-match p v e)))
                 env
                 elem-pats
                 val))]
    ; ctor(..)
    [(p-app _ ctor arg-pats)
     (and (val-data? val)
          (symbol=? (car ctor)
                    (vector-ref val 0))
          (foldl (lambda (p v e)
                   (and e (pattern-match p v e)))
                 env
                 arg-pats
                 (for/list ([i (in-naturals 1)]
                            [_ (in-list arg-pats)])
                   (vector-ref val i))))]))




;;;; TESTS ;;;;

(test-case "literal matching"
  (check-equal? (pattern-match 4 4 '(E)) '(E))
  (check-equal? (pattern-match 4 5 '(E)) #f)
  (check-equal? (pattern-match "a" "a" '(E)) '(E))
  (check-equal? (pattern-match "a" "b" '(E)) #f)
  (check-equal? (pattern-match "a" 5 '(E)) #f)
  )

(test-case "variable matching"
  (check-equal? (pattern-match (p-var #f 'x) 4 '(E)) '((x . 4) E))
  (check-equal? (pattern-match (p-var #f 'y) 5 '(E)) '((y . 5) E))
  )

(test-case "tuple matching"
  (check-equal? (pattern-match (p-tuple #f (list (p-var #f 'x) (p-var #f 'y)))
                               (list 4 5)
                               '(E))
                '((y . 5) (x . 4) E))
  (check-equal? (pattern-match (p-tuple #f (list (p-var #f 'x) 5))
                               (list 4 5)
                               '(E))
                '((x . 4) E))
  (check-equal? (pattern-match (p-tuple #f (list 4 (p-var #f 'y)))
                               (list 4 5)
                               '(E))
                '((y . 5) E))
  (check-equal? (pattern-match (p-tuple #f (list (p-var #f 'x) 6))
                               (list 4 5)
                               '(E))
                #f)
  (check-equal? (pattern-match (p-tuple #f (list 6 (p-var #f 'y)))
                               (list 4 5)
                               '(E))
                #f)
  )

(test-case "eval literal"
  (for ([l (in-list '(1 2 3 4 5 "a" "b" "c" #t #f))])
    (check-equal? (evaluate l) l)))

(test-case "eval var"
  (check-equal? (evaluate (e-var #f 'x) '((y . 4) (x . 5))) 5)
  (check-equal? (evaluate (e-var #f 'x) '((x . 5) (x . 6))) 5)
  (check-equal? (evaluate (e-var #f 'x) '((x . 5) (x . 6))) 5)
  (check-exn exn:fail? (thunk (evaluate (e-var #f 'x) '((y . 4)))))
  )

(test-case "eval tuple"
  (check-equal? (evaluate (e-tuple #f '(1 2 3)) '()) '(1 2 3))
  (check-equal? (evaluate (e-tuple #f (list 4 5 (e-var #f 'x)))
                          '((x . 6)))
                '(4 5 6))
  (check-equal? (evaluate (e-tuple #f (list 1 2 (e-tuple #f '(3 4)))))
                '(1 2 (3 4)))
  (check-equal? (evaluate (e-tuple #f '())) '())
  (check-equal? (evaluate (e-tuple #f '(3))) '3)
  )

(test-case "eval app"
  (define (f1 x) (add1 x))
  (define (f2 x) (+ (first x) (second x)))
  (check-equal? (evaluate (e-app #f (e-var #f 'f1) 4)
                          (list (cons 'f1 f1)))
                5)
  (check-equal? (evaluate (e-app #f (e-var #f 'f1) (e-var #f 'x))
                          (list (cons 'f1 f1) (cons 'x 5)))
                6)
  (check-equal? (evaluate (e-app #f (e-var #f 'f1) (e-var #f 'x))
                          (list (cons 'x 6) (cons 'f1 f1)))
                7)
  (check-equal? (evaluate (e-app #f (e-var #f 'f1)
                                 (e-app #f (e-var #f 'f1) (e-var #f 'x)))
                          (list (cons 'x 6) (cons 'f1 f1)))
                8)
  (check-equal? (evaluate (e-app #f (e-var #f 'f2)
                                 (e-tuple #f '(4 5)))
                          (list (cons 'f2 f2)))
                9)
  (check-exn exn:fail? (lambda () (evaluate (e-app #f 0 10))))
  )
