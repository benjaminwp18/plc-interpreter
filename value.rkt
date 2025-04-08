#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 3: Imperative Language Interpreter
;;;; ***************************************************

(require "binding.rkt" "common.rkt" "state.rkt")
(provide value-generic value-func-call)

; get value of expression, assuming expression uses a binary operator
(define (value-binary-operator expression state next throw)
  (value-generic (first-operand-literal expression) state
                 (lambda (op1)
                   (value-generic (second-operand-literal expression) state
                                  (lambda (op2)
                                    (let ([op (operator expression)])
                                      (cond
                                        [(eq? '+  op) (next (op-plus   op1 op2))]
                                        [(eq? '-  op) (next (op-minus  op1 op2))]
                                        [(eq? '*  op) (next (op-times  op1 op2))]
                                        [(eq? '/  op) (next (op-divide op1 op2))]
                                        [(eq? '%  op) (next (op-modulo op1 op2))]
                                        [(eq? '== op) (next (cond-eq   op1 op2))]
                                        [(eq? '!= op) (next (cond-neq  op1 op2))]
                                        [(eq? '>  op) (next (cond-gt   op1 op2))]
                                        [(eq? '<  op) (next (cond-lt   op1 op2))]
                                        [(eq? '<= op) (next (cond-leq  op1 op2))]
                                        [(eq? '>= op) (next (cond-geq  op1 op2))]
                                        [(eq? '&& op) (next (bool-and  op1 op2 throw))]
                                        [(eq? '|| op) (next (bool-or   op1 op2 throw))]
                                        [else (throw (~a "Invalid binary operator: " op) state)]))) throw))
                 throw))

; get value of expression, assuming expression uses a unary operator
(define (value-unary-operator expression state next throw)
  (value-generic (first-operand-literal expression) state
                 (lambda (op1)
                   (let ([op (operator expression)])
                     (cond
                       [(eq? '- op)  (next (op-unary-minus op1 throw))]
                       [(eq? '! op)  (next (bool-not       op1 throw))]
                       [else (throw (~a "Invalid unary operator: " op))])))
                 throw))

; get value of a function call
(define (value-func-call func-call state handle-next next throw)
  (let ([closure (binding-lookup (func-call-name func-call) state)])
    (state-block (closure-body closure)
                 (bind-params (closure-formal-params closure)
                              (func-call-actual-params func-call)
                              (binding-push-layer ((closure-scope-func closure) state))
                              state
                              throw)
                 handle-next
                 (lambda (v) (next v))
                 (lambda (s) (throw (~a "Break outside of loop in function " (func-call-name func-call))))
                 (lambda (s) (throw (~a "Continue outside of loop in function " (func-call-name func-call))))
                 (lambda (e s) (throw e state)))))

; get the value of expression, regardless of type or operator aryness
(define (value-generic expression state next throw)
  (cond
    [(number? expression) (next expression)]
    [(boolean-literal? expression) (next expression)]
    [(eq? (binding-status expression state) binding-init) (next (binding-lookup expression state))]
    [(eq? (binding-status expression state) binding-uninit) (error (~a expression " has not been assigned a value"))]
    [(not (pair? expression)) (error (~a expression " has not been declared"))]
    [(eq? (expr-start expression) 'funcall)
     (value-func-call (expr-func-call expression)
                      state
                      (lambda (s) (throw (~a "No return statement in function " (func-call-name (expr-func-call expression))) state))
                      next
                      throw)]
    [(has-second-operand? expression) (value-binary-operator expression state next throw)]
    [(has-first-operand? expression) (value-unary-operator expression state next throw)]
    [else (throw (~a "Invalid operator: " (operator expression)))]))

; ======================================================
; Operations

; Create binary boolean condition function that returns atom 'true/'false from racket function that
;  returns #t/#f
(define (build-condition racket-op)
  (lambda (op1 op2)
    (if (racket-op op1 op2)
        'true
        'false)))

; Create function that runs racket-op on two inputs but errors if either input is not an integer
; op-atom is an atom that describes the operation for use in error printing
(define (typesafe-binary-int-op racket-op op-atom)
  (typesafe-binary-op racket-op op-atom integer? 'integer))

; Create function that runs racket-op on one input but errors if the input is not an integer
; op-atom is an atom that describes the operation for use in error printing
(define (typesafe-unary-int-op racket-op op-atom)
  (typesafe-unary-op racket-op op-atom integer? 'integer))

; Create function that runs racket-op on two inputs but errors if either input fails predicate
; op-atom & predicate-atom are atoms that describe the operation & predicate for use in error printing
(define (typesafe-binary-op racket-op op-atom predicate predicate-atom)
  (lambda (op1 op2)
    (cond
      [(not (predicate op1)) (error (~a "Invalid left hand side of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [(not (predicate op2)) (error (~a "Invalid right hand side of operator " op-atom ": expected " predicate-atom ", got " op2))]
      [else (racket-op op1 op2)])))

; Create function that runs racket-op on one input but errors if the input fails predicate
; op-atom & predicate-atom are atoms that describe the operation & predicate for use in error printing
(define (typesafe-unary-op racket-op op-atom predicate predicate-atom)
  (lambda (op1 throw)
    (cond
      [(not (predicate op1)) (throw (~a "Invalid operand of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [else (racket-op op1)])))

(define cond-eq  (build-condition equal?))
(define cond-neq (build-condition not-equal?))
(define cond-gt  (build-condition (typesafe-binary-int-op > '>)))
(define cond-lt  (build-condition (typesafe-binary-int-op < '<)))
(define cond-geq (build-condition (typesafe-binary-int-op >= '>=)))
(define cond-leq (build-condition (typesafe-binary-int-op <= '<=)))

(define op-plus   (typesafe-binary-int-op + '+))
(define op-minus  (typesafe-binary-int-op - '-))
(define op-times  (typesafe-binary-int-op * '*))
(define op-divide (typesafe-binary-int-op quotient '/))
(define op-modulo (typesafe-binary-int-op remainder '%))

(define op-unary-minus (typesafe-unary-int-op (lambda (v) (- 0 v)) "unary -"))

; boolean not using 'true/'false atoms
; errors on operand that isn't a boolean atom
(define (bool-not op1 throw)
  (cond
    [(eq? op1 'true) 'false]
    [(eq? op1 'false) 'true]
    [else (throw "Inversion can only be applied to booleans")]))

; boolean and using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-and op1 op2 throw)
  (cond
    [(eq?        op1 'false)  'false]
    [(not-equal? op1 'true) (throw (~a "And can only be applied to booleans, got " op1))]
    [(eq?        op2 'false)  'false]
    [(not-equal? op2 'true) (throw (~a "And can only be applied to booleans, got " op2))]
    [else 'true]))

; boolean or using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-or op1 op2 throw)
  (cond
    [(eq?        op1 'true)  'true]
    [(not-equal? op1 'false) (throw (~a "Or can only be applied to booleans, got " op1))]
    [(eq?        op2 'true)  'true]
    [(not-equal? op2 'false) (throw (~a "Or can only be applied to booleans, got " op2))]
    [else 'false]))

; ======================================================
; Value Abstractions

; return true if expression is a boolean atom ('true or 'false)
(define (boolean-literal? expression) (or (eq? expression 'true) (eq? expression 'false)))

(define (has-second-operand? expression) (not-null? (cddr expression)))
(define (has-first-operand? expression) (not-null? (cdr expression)))

(define operator car)
(define first-operand-literal cadr)
(define second-operand-literal caddr)

(define first-param car)
(define next-params cdr)
(define expr-start car)
(define expr-func-call cdr)
(define func-call-name car)
(define func-call-actual-params cdr)
(define closure-formal-params car)
(define closure-body cadr)
(define closure-scope-func caddr)