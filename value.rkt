#lang racket

(provide value-generic)
(require "binding.rkt")

; value-int is a function that handles +, -, *, /, %, and the unary -
(define (value-int expression state)
   (cond
     [(number? expression) expression]
     [(eq? '+ (operator expression)) (+         (operand1 expression state) (operand2 expression state))]
     [(eq? '- (operator expression))            (helper-minus expression state)]
     [(eq? '* (operator expression)) (*         (operand1 expression state) (operand2 expression state))]
     [(eq? '/ (operator expression)) (quotient  (operand1 expression state) (operand2 expression state))]
     [(eq? '% (operator expression)) (remainder (operand1 expression state) (operand2 expression state))]
     [else (error ' bad-op "Invalid Operator")]))

; value-boolean is a function that handles ==, !=, >, <, <=, >=, &&, ||, !
(define (value-boolean expression state)
    (cond
      [(boolean-type? expression) expression]
      [(eq? '== (operator expression)) (equals expression state)]
      [(eq? '!= (operator expression)) (not-equals expression state)]
      [(eq? '>  (operator expression)) (greater-than expression state)]
      [(eq? '<  (operator expression)) (less-than expression state)]
      [(eq? '<= (operator expression)) (less-than-equals expression state)]
      [(eq? '>= (operator expression)) (greater-than-equals expression state)]
      [(eq? '&& (operator expression)) (short-circuit-and expression state)]
      [(eq? '|| (operator expression)) (short-circuit-or  expression state)]
      [(eq? '!  (operator expression)) (opposite expression state)]
      [else (error ' bad-op "Invalid Operator")]))

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

; ====================================
; helper functions

; helper-minus is a helper function to make distinct operations for unary minus and subtraction. it checks if there is three elements in the list to perform subtraction, and does unary instead if there is only two elements
(define (helper-minus expression state)
    (if (helper-is-binary expression) (- 0 (operand1 expression state))
        (-         (operand1 expression state) (operand2 expression state))))

; helper-is-binary is a helper function to check if a list contains more than two elements
(define (helper-is-binary expression)
    (if (null? (third-element-null-check expression)) #t
        #f))

; function that implements = expression but returns booleans in string form
(define (equals expression state)
    (if (eq? (operand1 expression state) (operand2 expression state)) 'true
      'false))

; function that implements != expression but returns booleans in string form
(define (not-equals expression state)
    (if (eq? (operand1 expression state) (operand2 expression state)) 'false
      'true))

; function that implements > expression but returns booleans in string form
(define (greater-than expression state)
    (if (> (operand1 expression state) (operand2 expression state)) 'true
      'false))

; function that implements < expression but returns booleans in string form
(define (less-than expression state)
    (if (< (operand1 expression state) (operand2 expression state)) 'true
      'false))

; function that implements >= expression but returns booleans in string form
(define (greater-than-equals expression state)
    (if (>= (operand1 expression state) (operand2 expression state)) 'true
      'false))

; function that implements <= expression but returns booleans in string form
(define (less-than-equals expression state)
    (if (<= (operand1 expression state) (operand2 expression state)) 'true
      'false))

; function that implements ! expression but returns booleans in string form
(define (opposite expression state)
    (cond
      ((eq? (operand1 expression state) 'true) 'false)
      ((eq? (operand1 expression state) 'false) 'true)
      (else (error "must input boolean value"))))

; version of and function with explicit short circuiting
(define (short-circuit-and expression state)
    (cond
      [(eq? 'false (operand1 expression state))  'false]
      [(eq? 'false (operand2 expression state)) 'false]
      [else 'true]))

; version of or function with explicit short circuiting
(define (short-circuit-or expression state)
    (cond
      [(eq? 'true (operand1 expression state))  'true]
      [(eq? 'true (operand2 expression state)) 'true]
      [else 'false]))

; function that returns if the boolean value is in string form ('true or 'false)
(define (boolean-type? expression)
  (or (eq? expression 'true) (eq? expression 'false)))

; in-list takes an atom and a list and returns true if the element is in the list and false if the element is not in the list
(define (in-list? a lis)
    (cond
      [(null? lis)       #f]
      [(eq? a (car lis)) #t]
      [else             (in-list? a (cdr lis))]))

; ====================================
;abstractions
(define operator car)
(define first-operand cadr)
(define second-operand caddr)
(define third-element-null-check cddr)
(define (operand1 expression state)
  (value-generic (first-operand expression) state))

(define (operand2 expression state)
  (value-generic (second-operand expression) state))