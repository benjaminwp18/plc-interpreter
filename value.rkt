#lang racket

(require "binding.rkt" "common.rkt")
(provide value-generic)

(define (value-binary-operator expression state)
  (let ([op (operator expression)]
        [op1 (operand1 expression state)]
        [op2 (operand2 expression state)])
    (cond
      [(eq? '+  op) (op-plus   op1 op2)]
      [(eq? '-  op) (op-minus  op1 op2)]
      [(eq? '*  op) (op-times  op1 op2)]
      [(eq? '/  op) (op-divide op1 op2)]
      [(eq? '%  op) (op-modulo op1 op2)]
      [(eq? '== op) (cond-eq   op1 op2)]
      [(eq? '!= op) (cond-neq  op1 op2)]
      [(eq? '>  op) (cond-gt   op1 op2)]
      [(eq? '<  op) (cond-lt   op1 op2)]
      [(eq? '<= op) (cond-leq  op1 op2)]
      [(eq? '>= op) (cond-geq  op1 op2)]
      [(eq? '&& op) (bool-and  op1 op2)]
      [(eq? '|| op) (bool-or   op1 op2)]
      [else (error (~a "Invalid binary operator: " op))])))

(define (value-unary-operator expression state)
  (let ([op (operator expression)]
        [op1 (operand1 expression state)])
    (cond
      [(eq? '- op)  (op-unary-minus op1)]
      [(eq? '! op)  (bool-not       op1)]
      [else (error (~a "Invalid unary operator: " op))])))

; value-generic is a function to determine if an expression needs to be handled by value-boolean or value-int
(define (value-generic expression state)
  (cond
    [(number? expression) expression]
    [(boolean-literal? expression) expression]
    [(eq? (binding-status expression state) binding-init) (binding-lookup expression state)]
    [(eq? (binding-status expression state) binding-uninit) (error (~a expression " has not been assigned a value"))]
    [(not (pair? expression)) (error (~a expression " has not been declared"))]
    [(has-second-operand? expression) (value-binary-operator expression state)]
    [(has-first-operand? expression) (value-unary-operator expression state)]
    [else (error (~a "Invalid operator: " (operator expression)))]))

; ====================================
; helper functions

(define (typesafe-unary-op racket-op op-atom predicate predicate-atom)
  (lambda (op1)
    (cond
      [(not (predicate op1)) (error (~a "Invalid operand of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [else (racket-op op1)])))

(define (typesafe-binary-op racket-op op-atom predicate predicate-atom)
  (lambda (op1 op2)
    (cond
      [(not (predicate op1)) (error (~a "Invalid left hand side of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [(not (predicate op2)) (error (~a "Invalid right hand side of operator " op-atom ": expected " predicate-atom ", got " op2))]
      [else (racket-op op1 op2)])))

(define (typesafe-binary-int-op racket-op op-atom)
  (typesafe-binary-op racket-op op-atom integer? 'integer))

(define (typesafe-unary-int-op racket-op op-atom)
  (typesafe-unary-op racket-op op-atom integer? 'integer))

(define (build-condition racket-op)
  (lambda (op1 op2)
    (if (racket-op op1 op2)
        'true
        'false)))

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

; return true if expression is a boolean atom ('true or 'false)
(define (boolean-literal? expression)
  (or (eq? expression 'true) (eq? expression 'false)))

; ====================================
; abstractions
(define (operand1 expression state)
  (value-generic (first-operand-literal expression) state))
(define (operand2 expression state)
  (value-generic (second-operand-literal expression) state))
(define (has-second-operand? expression) (not-null? (cddr expression)))
(define (has-first-operand? expression) (not-null? (cdr expression)))

(define operator car)
(define first-operand-literal cadr)
(define second-operand-literal caddr)
