#lang racket


(define (main)
  (for ([e (in-list
            (list
             3                    ;; - 3 : Int
             #t                   ;; - true : Bool
             (lam 'x 'x)          ;; - <function> : (-> 'a 'a)
             (lam* 'x 'y 'x)      ;; - <function> : (-> 'a (-> 'b 'a))
             (app (lam 'x 'x)     ;; - 100 : Int
                  100)
             (app* '+ 5 7)        ;; - 12 : Int
             (app* '< 5 7)        ;; - true : Bool
             'nil                 ;; - [] : Listof('a)
             (let-in 'f (lam 'x 'x) ;; - 3 : Int
                     (app 'f 3))
             (let-in 'f (lam 'x 'x) ;; - [3] : Listof(Int)
                     (app* 'cons
                           (app 'f 3)
                           (app 'f 'nil)))
             ))])
    (displayln (typecheck/eval e))))

(define (typecheck/eval e)
  (let* ([e+t (annotate e)]
         [t (typecheck e+t)]
         [v (evaluate e)])
    (format "- ~a : ~a"
            (val->string v)
            (type->string t))))


;;;; Misc. functions ;;;;

;;; Add record to assoc list.
(define (+alist x v al)
  (cons (cons x v) al))

;; (lookup [x : variable?] [al : (listof (cons/c variable? V))]) -> V
(define (lookup x al)
  (cdr (or (assoc x al variable=?)
           (error (format "undefined variable \"~a\"" x)))))



;;;; Expression syntax ;;;;

;; lambda calculus (with let)
(define variable? symbol?)
(struct app (fn arg) #:transparent)
(struct lam (var body) #:transparent)
(struct let-in (var rhs body) #:transparent)

(define expr?
  (or/c integer?
        boolean?
        variable?
        app?
        lam?
        let-in?))

(define variable=? symbol=?)

;;; Utility functions for building chained expressions
;;; e.g. (app* e1 e2 e3) = (app (app e1 e2) e3)
;;;      (lam* x y e)    = (lam x (lam y e))
;; (app* expr? ...) -> expr?
(define (app* f . es)
  (foldl (lambda (e2 e1) (app e1 e2)) f es))
;; (lam* symbol? ... expr?) -> expr?
(define (lam* . l)
  (match l
    [(list xs ... e)
     (foldr lam e xs)]))




;;;; Values and eval ;;;;

(define val?
  (or/c integer?
        boolean?
        list?
        procedure?))

(define env?
  (listof (cons/c variable? val?)))

;; standard-env : env?
(define standard-env
  (list
   (cons '+ (lambda (x) (lambda (y) (+ x y))))
   (cons '< (lambda (x) (lambda (y) (< x y))))
   (cons 'cons (lambda (x) (lambda (xs) (cons x xs))))
   (cons 'nil '())
   (cons 'foldr (lambda (f) (lambda (z) (lambda (a) (foldr f z a)))))
   ))

;;; Evaluate an expression
;; (evaluate [e : expr?]
;;           [env : env? = standard-env])
;;   -> val?
(define (evaluate e [env standard-env])
  (match e
    ; constants
    [(? integer? i) i]
    [(? boolean? b) b]
    ; variable
    [(? variable? x) (lookup x env)]
    ; application
    [(app fn arg) (apply (evaluate fn env)
                         (list (evaluate arg env)))]
    ; abstraction
    [(lam x body)
     (lambda (v) (evaluate body (+alist x v env)))]
    ; let binding
    [(let-in x rhs body)
     (evaluate body (+alist x (evaluate rhs env) env))]))




;;;; Type syntax ;;;;

(struct fun (arg ret) #:transparent)
(struct data (name args) #:transparent)
(struct forall (var type) #:transparent)

(define type?
  (or/c variable?
        fun?
        data?))

(define schema?
  (or/c type?
        forall?))

(define type-ctx?
  (listof (cons/c variable? schema?)))

;;; Utility functions for types
;; (fun* type? ... type?) -> type?
(define (fun* . l)
  (match l
    [(list args ... ret)
     (foldr fun ret args)]))
;; (forall* symbol? ... type?) -> schema?
(define (forall* . l)
  (match l
    [(list vars ... type)
     (foldr forall type vars)]))

;;; Basic data types
(define Int (data 'Int '()))
(define Bool (data 'Bool '()))
(define (Listof x) (data 'Listof (list x)))

;;; Standard context (types of values in standard-env)
(define standard-ctx
  (let ([-> fun*])
    (list
     (cons '+ (-> Int Int Int))
     (cons '< (-> Int Int Bool))
     (cons 'nil (forall 'A (Listof 'A)))
     (cons 'cons (forall 'A (-> 'A (Listof 'A) (Listof 'A))))
     (cons 'foldr (forall* 'A 'B (-> (-> 'A 'B 'B) 'B (Listof 'A) 'B)))
     )))




;;;; Annotated expression syntax ;;;;

;;; Note: by deriving from the untyped expression structs,
;;;  we don't have to write a new evaluator for typed expressions.
;;; Edit: the above is not true because standard-env is not type annotated.
; Type application & Type lambdas:
(struct t-app app () #:transparent)
(struct t-lam lam () #:transparent)
; Lambda with type annotated argument:
(struct lam+t lam (var-type) #:transparent)

(define expr+t?
  (or/c integer?
        boolean?
        variable?
        t-app?
        t-lam?
        app?
        lam+t?
        let-in?))




;;;; Type inference ;;;;

;;; Annotate an untyped expression by inferring the missing types.
;; (annotate [e : expr?]
;;           [ctx : type-ctx? = standard-ctx])
;;   -> expr+t?
(define (annotate e [ctx standard-ctx])
  (let*-values ([(e+t e/t C) (infer e ctx)]
                [(S) (solve C)])
    (subst-apply/expr+t S e+t)))

;;; Substitutions
;;; Note: Newer substitutions appear before older substitutions.
;;;  e.g. (free-vars (cdar S)) and (map car S) are disjoint.
(define subst?
  (listof (cons/c variable? type?)))

;;; Apply substitutions in S to type / context, replacing all
;;;  occurences of variables in S with associated new types.
;; (subst-apply [S : subst?] [ctx : type-ctx?]) -> type-ctx?
;; (subst-apply [S : subst?] [t : type?]) -> type?
(define (subst-apply S o)
  (match o
    ; context
    ['() '()]
    [(cons (cons x scma) ctx)
     (cons (cons x (subst-apply S scma)) (subst-apply S ctx))]
    ; type
    [(fun t1 t2)
     (fun (subst-apply S t1)
          (subst-apply S t2))]
    [(data d tys)
     (data d (map (lambda (t) (subst-apply S t)) tys))]
    [(forall x scma)
     (forall x (subst-apply S scma))]
    [(? symbol? x)
     (match (assoc x S variable=?)
       [#f x]
       [(cons _ t) (subst-apply S t)])]))
;;; Apply "shallow substitution" to type, only substituting for immediate variables.
;;;  (no recursive substitution).
;; (subst-apply/shallow [S : subst?] [t : type?]) -> type?
(define (subst-apply/shallow S t)
  (match t
    [(? variable? x)
     (match (assoc x S variable=?)
       [#f x]
       [(cons _ t2) (subst-apply/shallow S t2)])]
    [_ t]))

;;; Apply substitutions to typed expression.
;; (subst-apply/expr+t [S : subst?] [e+t : expt+t?]) -> type?
(define (subst-apply/expr+t S e+t)
  (match e+t
    ; type application / abstraction
    [(t-app fn arg)
     (t-app (subst-apply/expr+t S fn)
            (subst-apply S arg))]
    [(t-lam x body)
     (t-lam x (subst-apply/expr+t S body))]
    ; typed lambda
    [(lam+t x body x/t)
     (lam+t x
            (subst-apply/expr+t S body)
            (subst-apply S x/t))]
    ; application
    [(app fn arg)
     (app (subst-apply/expr+t S fn)
          (subst-apply/expr+t S arg))]
    ; let binding
    [(let-in x rhs body)
     (let-in x
             (subst-apply/expr+t S rhs)
             (subst-apply/expr+t S body))]
    [_ e+t]))




;;; Constraints
(struct cns= (type1 type2) #:transparent)
(define cns? cns=?)

;;; Solve the given constraints, returning solution substitutions.
;; (solve [C : (listof cns?)]) -> subst?
(define (solve C)
  (foldl (lambda (c S) (match c
                    [(cns= t1 t2)
                     (unify t1 t2 S)]))
         '()
         C))

;;; Unification of types, accumulating into substitutions S.
;;; Throws exception if types impossible to unify (incompatible).
;; (unify [t1 : type?] [t2 : type?] [S : subst?]) -> subst?
(define (unify t1 t2 S)
  (match* ((subst-apply/shallow S t1)
           (subst-apply/shallow S t2))
    ; arg -> ret
    [((fun arg1 ret1) (fun arg2 ret2))
     (unify ret1 ret2 (unify arg1 arg2 S))]
    ; data type
    [((data d args1) (data d args2))
     (foldl unify S args1 args2)]
    ; variable
    [((? variable? x) t) (cons (cons x t) S)]
    [(t (? variable? x)) (cons (cons x t) S)]
    ; error
    [(_ _)
     (let ([los (types->strings (list (subst-apply S t1)
                                     (subst-apply S t2)))])
       (error (format "incompatible types: `~a' vs `~a'"
                      (first los)
                      (second los))))]))

;;; Type inference algorithm; infers the type, annotates the
;;;  expression, and gives a list of constraints to be fulfilled.
;; (infer [e : expr?] [ctx : type-ctx?])
;;   -> expr+t?
;;      type?
;;      (listof cns?)
(define (infer e ctx)
  (match e
    ; constants
    [(? integer? i) (values i Int '())]
    [(? boolean? b) (values b Bool '())]
    ; variable
    [(? variable? x)
     (let*-values ([(vars t) (schema-bound-vars (lookup x ctx))]
                   ; generate new type variables
                   [(new-vars) (map (lambda (_) (new-type)) vars)]
                   ; instantiate type with new variables
                   [(e/t) (subst-apply (map cons vars new-vars) t)])
       (values (foldl (lambda (var e) (t-app e var)) x new-vars)
               e/t
               '()))]

    ; application
    [(app fn arg)
     (let*-values ([(fn+t fn/t C1) (infer fn ctx)]
                   [(arg+t arg/t C2) (infer arg ctx)]
                   [(ret/t) (new-type)])
       (values (app fn+t arg+t)
               ret/t
               (cons (cns= fn/t (fun arg/t ret/t))
                     (append C2 C1))))]
    ; abstraction
    [(lam x body)
     (let*-values ([(var/t) (new-type)]
                   [(body+t body/t C) (infer body (+alist x var/t ctx))])
       (values (lam+t x body+t var/t)
               (fun var/t body/t)
               C))]

    ; let binding
    [(let-in x rhs body)
     (let*-values ([(rhs+t rhs/t C1) (infer rhs ctx)]
                   ; generalize rhs type by prematurely solving these constraints
                   [(S) (solve C1)]
                   [(rhs/scma) (generalize (subst-apply S rhs/t)
                                           (subst-apply S ctx))]
                   ; infer body with generalized type in ctx
                   [(body+t body/t C2) (infer body (+alist x rhs/scma ctx))])
       (values (let-in x
                       (let-values ([(vars _) (schema-bound-vars rhs/scma)])
                         (foldr t-lam rhs+t vars))
                       body+t)
               body/t
               (append C2 C1)))]
    ))

;;; Retrieve all type variables from schema, in the same
;;;  order that they occur.
;;; e.g. (schema-bound-vars (forall 'A (forall 'B 'T))) -> '(A B) 'T
;; (schema-bound-vars [scma : schema?])
;;   -> (listof variable?)
;;      type?
(define (schema-bound-vars scma)
  (let pull ([scma scma] [vars '()])
    (match scma
      [(forall x scma2) (pull scma2 (cons x vars))]
      [t (values (reverse vars) t)])))

;;; Generate unique type variable.
;; (new-type) -> type?
(define (new-type) (gensym 't:))

;;; Generalize free type variables in type (with context)
;; (generalize [t : type?] [ctx : type-ctx?]) -> schema?
(define (generalize t ctx)
  (let ([fv (foldl (lambda (p fv)
                     ; remove from 'fv' the free vars in this variable
                     (foldl remove fv
                            (free-vars (cdr p))))
                   (free-vars t)
                   ctx)])
    ; turn free vars into forall's
    (foldl forall t fv)))

;;; Get list of free variables in schema / type
;; (free-vars [scma : schema?]) -> (listof variable?)
(define (free-vars scma)
  (match scma
    [(forall x scma2)
     (remove x (free-vars scma2))]
    [(fun t1 t2)
     (remove-duplicates (append (free-vars t1) (free-vars t2))
                        variable=?)]
    [(data d ts)
     (remove-duplicates (foldl (lambda (t fv)
                                 (append fv (free-vars t)))
                               '() ts)
                        variable=?)]
    [(? variable? x)
     (list x)]))


;;;; Typechecking annotated expressions ;;;;
;; (typecheck [e+t : expr+t?]
;;            [ctx : type-ctx? = standard-ctx])
;;   -> schema?
(define (typecheck e+t [ctx standard-ctx])
  (match e+t
    [(? integer? i) Int]
    [(? boolean? b) Bool]
    [(? variable? x) (lookup x ctx)]
    [(t-app fn arg)
     (match (typecheck fn ctx)
       [(forall x scma)
        (subst-apply (list (cons x arg)) scma)]
       [t
        (error (format "invalid type application on non-generic type `~a'" t))])]
    [(t-lam x body)
     (forall x (typecheck body ctx))]
    [(app fn arg)
     (match (typecheck fn ctx)
       [(fun exp-arg/t ret/t)
        (let ([giv-arg/t (typecheck arg ctx)])
          (when (not (equal? exp-arg/t giv-arg/t))
            (let ([los (types->strings (list exp-arg/t giv-arg/t))])
              (error (format "expected argument of type `~a', got `~a'"
                             (first los)
                             (second los)))))
          ret/t)]
       [t
        (error (format "cannot apply value of non-function type `~a'"
                       (type->string t)))])]
    [(lam+t x body arg)
     (fun arg (typecheck body (+alist x arg ctx)))]
    [(let-in x rhs body)
     (typecheck body (+alist x (typecheck rhs ctx)
                             ctx))]
    ))

;;; Convert types to pretty string representations
;; (type->string [t : type?]) -> string?
;; (types->strings [l : (listof type?)]) -> string?
(define (type->string t) (first (types->strings (list t))))
(define (types->strings l)
  (define var-names (make-hash))
  (define next-name
    (let ([i 0])
      (lambda ()
        (set! i (add1 i))
        (string (string-ref "abcdefghijklmnopqrstuvwxyz"
                            (sub1 i))))))
  (define (->s t)
    (match t
      [(fun t1 t2) (format "(-> ~a ~a)"
                           (->s t1)
                           (->s t2))]
      [(data d ts)
         (if (empty? ts)
             (symbol->string d)
             (format "~a(~a)"
                     d
                     (string-join (map ->s ts) ", ")))]
      [(? variable? x)
       (format "'~a"
               (hash-ref! var-names x next-name))]))
  (map ->s l))

;;; Convert value to pretty string representation
;; (val->string [v : val?]) -> String?
(define (val->string v)
  (match v
    [(? procedure?) "<function>"]
    [(? list? vs)
     (format "[~a]"
             (string-join (map val->string vs) ", "))]
    [#t "true"]
    [#f "false"]
    [(? integer? i) (number->string i)]))
