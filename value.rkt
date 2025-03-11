#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 2: Flow Control Interpreter
;;;; ***************************************************

(require "binding.rkt" "common.rkt")
(provide value-generic)

; get value of expression, assuming expression uses a binary operator
(define (value-binary-operator expression state return)
  (value-generic (operand2 expression state return) state (lambda (op2)
    (value-generic (operand1 expression state return) state (lambda (op1)
      (let ([op (operator expression)])
      (cond
        [(eq? '+  op) (return (op-plus   op1 op2))]
        [(eq? '-  op) (return (op-minus  op1 op2))]
        [(eq? '*  op) (return (op-times  op1 op2))]
        [(eq? '/  op) (return (op-divide op1 op2))]
        [(eq? '%  op) (return (op-modulo op1 op2))]
        [(eq? '== op) (return (cond-eq   op1 op2))]
        [(eq? '!= op) (return (cond-neq  op1 op2))]
        [(eq? '>  op) (return (cond-gt   op1 op2))]
        [(eq? '<  op) (return (cond-lt   op1 op2))]
        [(eq? '<= op) (return (cond-leq  op1 op2))]
        [(eq? '>= op) (return (cond-geq  op1 op2))]
        [(eq? '&& op) (return (bool-and  op1 op2))]
        [(eq? '|| op) (return (bool-or   op1 op2))]
        [else (error (~a "Invalid binary operator: " op))])))))))

; get value of expression, assuming expression uses a unary operator
(define (value-unary-operator expression state return)
  (value-generic (operand1 expression state return) state (lambda (op1)
    (let ([op (operator expression)])
      (cond
        [(eq? '- op)  (return (op-unary-minus op1))]
        [(eq? '! op)  (return (bool-not       op1))]
        [else (error (~a "Invalid unary operator: " op))])))))

; get the value of expression, regardless of type or operator aryness
(define (value-generic expression state return)
  (cond
    [(number? expression) (return expression)]
    [(boolean-literal? expression) (return expression)]
    [(eq? (binding-status expression state) binding-init) (return (binding-lookup expression state))]
    [(eq? (binding-status expression state) binding-uninit) (error (~a expression " has not been assigned a value"))]
    [(not (pair? expression)) (error (~a expression " has not been declared"))]
    [(has-second-operand? expression) (value-binary-operator expression state return)]
    [(has-first-operand? expression) (value-unary-operator expression state return)]
    [else (error (~a "Invalid operator: " (operator expression)))]))


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
  (lambda (op1)
    (cond
      [(not (predicate op1)) (error (~a "Invalid operand of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [else (racket-op op1)])))

; ====================================
; Operations

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
(define (bool-not op1)
  (cond
    [(eq? op1 'true) 'false]
    [(eq? op1 'false) 'true]
    [else (error "Inversion can only be applied to booleans")]))

; boolean and using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-and op1 op2)
  (cond
    [(eq?        op1 'false)  'false]
    [(not-equal? op1 'true) (error (~a "And can only be applied to booleans, got " op1))]
    [(eq?        op2 'false)  'false]
    [(not-equal? op2 'true) (error (~a "And can only be applied to booleans, got " op2))]
    [else 'true]))

; boolean and using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-or op1 op2)
  (cond
    [(eq?        op1 'true)  'true]
    [(not-equal? op1 'false) (error (~a "Or can only be applied to booleans, got " op1))]
    [(eq?        op2 'true)  'true]
    [(not-equal? op2 'false) (error (~a "Or can only be applied to booleans, got " op2))]
    [else 'false]))

; ====================================
; Abstractions

; return true if expression is a boolean atom ('true or 'false)
(define (boolean-literal? expression) (or (eq? expression 'true) (eq? expression 'false)))

(define (operand1 expression state return) (value-generic (first-operand-literal expression) state return))
(define (operand2 expression state return) (value-generic (second-operand-literal expression) state return))
(define (has-second-operand? expression) (not-null? (cddr expression)))
(define (has-first-operand? expression) (not-null? (cdr expression)))

(define operator car)
(define first-operand-literal cadr)
(define second-operand-literal caddr)
