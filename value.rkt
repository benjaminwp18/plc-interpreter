#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 1: Simple Language Interpreter
;;;; ***************************************************

(provide value-generic)
(require "binding.rkt")
(require "common.rkt")

; value-int is a function that handles +, -, *, /, %, and the unary -
(define (value-int expression state)
  (cond
    [(number? expression) expression]
    [(eq? '+ (operator expression)) (+         (operand1 expression state) (operand2 expression state))]
    [(eq? '- (operator expression)) (minus-or-negative expression state)]
    [(eq? '* (operator expression)) (*         (operand1 expression state) (operand2 expression state))]
    [(eq? '/ (operator expression)) (quotient  (operand1 expression state) (operand2 expression state))]
    [(eq? '% (operator expression)) (remainder (operand1 expression state) (operand2 expression state))]
    [else (error (~a "Invalid integer operator: " (operator expression)))]))

; value-boolean is a function that handles ==, !=, >, call/cc  <, <=, >=, &&, ||, !
(define (value-boolean expression state)
  (cond
    [(boolean-literal? expression) expression]
    [(eq? '== (operator expression)) (cond-eq expression state)]
    [(eq? '!= (operator expression)) (cond-neq expression state)]
    [(eq? '>  (operator expression)) (cond-gt expression state)]
    [(eq? '<  (operator expression)) (cond-lt expression state)]
    [(eq? '<= (operator expression)) (cond-leq expression state)]
    [(eq? '>= (operator expression)) (cond-geq expression state)]
    [(eq? '&& (operator expression)) (bool-and expression state)]
    [(eq? '|| (operator expression)) (bool-or  expression state)]
    [(eq? '!  (operator expression)) (bool-not expression state)]
    [else (error (~a "Invalid boolean operator: " (operator expression)))]))

; value-generic is a function to determine if an expression needs to be handled by value-boolean or value-int
(define (value-generic expression state)
  (cond
    [(number? expression) (value-int expression state)]
    [(boolean-literal? expression) (value-boolean expression state)]
    [(eq? (binding-status expression state) binding-init) (binding-lookup expression state)]
    [(eq? (binding-status expression state) binding-uninit) (error (~a expression " has not been assigned a value"))]
    [(not (pair? expression)) (error (~a expression " has not been declared"))]
    [(in-list? (operator expression) '(+ - * / %)) (value-int expression state)]
    [(in-list? (operator expression) '(== != > < <= >= && || !)) (value-boolean expression state)]
    [else (error (~a "Invalid operator: " (operator expression)))]))

; ====================================
; helper functions

; if expression has 2 operands: return operand 1 - operand 2
; else: return negative operand1
(define (minus-or-negative expression state)
  (if (has-second-operand? expression)
      (- (operand1 expression state) (operand2 expression state))
      (- 0 (operand1 expression state))))

; helper to convert Racket boolean conditional racket-op to 'true/'false atom conditional
(define (build-condition expression state racket-op)
  (if (racket-op (operand1 expression state) (operand2 expression state))
      'true
      'false))

(define (cond-eq  expression state) (build-condition expression state equal?))
(define (cond-neq expression state) (build-condition expression state not-equal?))
(define (cond-gt  expression state) (build-condition expression state >))
(define (cond-lt  expression state) (build-condition expression state <))
(define (cond-geq expression state) (build-condition expression state >=))
(define (cond-leq expression state) (build-condition expression state <=))

; boolean not using 'true/'false atoms
; errors on operand that isn't a boolean atom
(define (bool-not expression state)
  (cond
    [(eq? (operand1 expression state) 'true) 'false]
    [(eq? (operand1 expression state) 'false) 'true]
    [else (error "Inversion can only be applied to booleans")]))

; boolean and using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-and expression state)
  (cond
    [(eq?        (operand1 expression state) 'false)  'false]
    [(not-equal? (operand1 expression state) 'true) (error (~a "And can only be applied to booleans, got " (operand1 expression state)))]
    [(eq?        (operand2 expression state) 'false)  'false]
    [(not-equal? (operand2 expression state) 'true) (error (~a "And can only be applied to booleans, got " (operand2 expression state)))]
    [else 'true]))

; boolean and using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-or expression state)
  (cond
    [(eq?        (operand1 expression state) 'true)  'true]
    [(not-equal? (operand1 expression state) 'false) (error (~a "Or can only be applied to booleans, got " (operand1 expression state)))]
    [(eq?        (operand2 expression state) 'true)  'true]
    [(not-equal? (operand2 expression state) 'false) (error (~a "Or can only be applied to booleans, got " (operand2 expression state)))]
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

(define operator car)
(define first-operand-literal cadr)
(define second-operand-literal caddr)
