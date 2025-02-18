#lang racket

(provide (all-defined-out))

(require "binding.rkt")

; value-int is a function that handles +, -, *, /, %, and the unary -
(define (value-int expression state)
   (cond
     [(number? expression) expression]
     [(eq? '+ (operator expression)) (+         (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
     [(eq? '- (operator expression))            (helper-minus expression state)]
     [(eq? '* (operator expression)) (*         (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
     [(eq? '/ (operator expression)) (quotient  (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
     [(eq? '% (operator expression)) (remainder (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
     [else (error ' bad-op "Invalid Operator")]))

; helper-minus is a helper function to make distinct operations for unary minus and subtraction. it checks if there is three elements in the list to perform subtraction, and does unary instead if there is only two elements
(define (helper-minus expression state)
    (if (helper-is-binary expression) (- 0 (value-generic (first-operand expression) state))
        (-         (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))))

; helper-is-binary is a helper function to check if a list contains more than two elements
(define (helper-is-binary expression)
    (if (null? (third-element-null-check expression)) #t
        #f))

; value-boolean is a function that handles ==, !=, >, <, <=, >=, &&, ||, !
(define (value-boolean expression state)
    (cond
      [(boolean-type? expression) (boolean-value expression)]
      [(eq? '== (operator expression)) (eq?         (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
      [(eq? '!= (operator expression)) (not (eq?    (value-generic (first-operand expression) state) (value-generic (second-operand expression) state)))]
      [(eq? '>  (operator expression)) (>           (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
      [(eq? '<  (operator expression)) (<           (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
      [(eq? '<= (operator expression)) (<=          (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
      [(eq? '>= (operator expression)) (>=          (value-generic (first-operand expression) state) (value-generic (second-operand expression) state))]
      [(eq? '&& (operator expression)) (short-circuit-and expression state)]
      [(eq? '|| (operator expression)) (short-circuit-or  expression state)]
      [(eq? '!  (operator expression)) (not         (value-generic (first-operand expression) state))]
      [else (error ' bad-op "Invalid Operator")]))

; version of and function with explicit short circuiting
(define (short-circuit-and expression state)
    (cond
      [(eq? #f (value-generic (first-operand expression) state))  #f]
      [(eq? #f (value-generic (second-operand expression) state)) #f]
      [else #t]))

; version of or function with explicit short circuiting
(define (short-circuit-or expression state)
    (cond
      [(eq? #t (value-generic (first-operand expression) state))  #t]
      [(eq? #t (value-generic (second-operand expression) state)) #t]
      [else #f]))

(define (boolean-type? expression)
  (or (eq? expression 'true) (eq? expression 'false)))

(define (boolean-value expression)
  (eq? expression 'true))

; value-generic is a function to determine if an expression needs to be handled by value-boolean or value-int
(define (value-generic expression state)
    (cond
      [(number? expression) (value-int expression state)]
      [(boolean-type? expression) (value-boolean expression state)]
      [(eq? (binding-status expression state) binding-init) (binding-lookup expression state)]
      [(eq? (binding-status expression state) binding-uninit) (error (~a expression " has not been assigned a value"))]
      [(not (pair? expression)) (error (~a expression " has not been declared"))]
      [(in-list? (operator expression) '(+ - * / %)) (value-int expression state)]
      [(in-list? (operator expression) '(== != > < <= >= && || !)) (value-boolean expression state)]
      [else (error ' bad-op "Invalid Operator")]))

; in-list takes an atom and a list and returns true if the element is in the list and false if the element is not in the list
(define (in-list? a lis)
    (cond
      [(null? lis)       #f]
      [(eq? a (car lis)) #t]
      [else             (in-list? a (cdr lis))]))

;abstractions
(define operator car)
(define first-operand cadr)
(define second-operand caddr)
(define third-element-null-check cddr)