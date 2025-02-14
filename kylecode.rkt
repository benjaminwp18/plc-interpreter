#lang racket

; Code written by Kyle


; value-int is a function that handles +, -, *, /, %, and the unary -
(define value-int
  (lambda (expression)
   (cond
     ((number? expression) expression)
     ((eq? '+ (operator expression)) (+         (value-generic(first-operand expression)) (value-generic(second-operand expression))))
     ((eq? '- (operator expression))            (helper-minus expression))
     ((eq? '* (operator expression)) (*         (value-generic(first-operand expression)) (value-generic(second-operand expression))))
     ((eq? '/ (operator expression)) (quotient  (value-generic(first-operand expression)) (value-generic(second-operand expression))))
     ((eq? '% (operator expression)) (remainder (value-generic(first-operand expression)) (value-generic(second-operand expression))))
     (else (error ' bad-op "Invalid Operator")))))

; helper-minus is a helper function to make distinct operations for unary minus and subtraction. it checks if there is three elements in the list to perform subtraction, and does unary instead if there is only two elements
(define helper-minus
  (lambda (expression)
    (if (helper-is-binary expression) (- 0 (value-generic (first-operand expression)))
        (-         (value-generic(first-operand expression)) (value-generic(second-operand expression))))))

; helper-is-binary is a helper function to check if a list contains more than two elements
(define helper-is-binary
  (lambda (expression)
    (if (null? (third-element-null-check expression)) #t
        #f)))

; value-boolean is a function that handles ==, !=, >, <, <=, >=, &&, ||, !
(define value-boolean
  (lambda (expression)
    (cond
      ((boolean? expression) expression)
      ((eq? '== (operator expression)) (eq?         (value-generic(first-operand expression)) (value-generic(second-operand expression))))
      ((eq? '!= (operator expression)) (not(eq?     (value-generic(first-operand expression)) (value-generic(second-operand expression)))))
      ((eq? '>  (operator expression)) (>           (value-generic(first-operand expression)) (value-generic(second-operand expression))))
      ((eq? '<  (operator expression)) (<           (value-generic(first-operand expression)) (value-generic(second-operand expression))))
      ((eq? '<= (operator expression)) (<=          (value-generic(first-operand expression)) (value-generic(second-operand expression))))
      ((eq? '>= (operator expression)) (>=          (value-generic(first-operand expression)) (value-generic(second-operand expression))))
      ((eq? '&& (operator expression)) (short-circuit-and expression))
      ((eq? '|| (operator expression)) (short-circuit-or  expression))
      ((eq? '!  (operator expression)) (not         (value-generic(first-operand expression))))
      (else (error ' bad-op "Invalid Operator")))))

; version of and function with explicit short circuiting
(define short-circuit-and
  (lambda (expression)
    (cond
      ((eq? #f (value-generic(first-operand expression)))  #f)
      ((eq? #f (value-generic(second-operand expression))) #f)
      (else #t))))

; version of or function with explicit short circuiting
(define short-circuit-or
  (lambda (expression)
    (cond
      ((eq? #t (value-generic(first-operand expression)))  #t)
      ((eq? #t (value-generic(second-operand expression))) #t)
      (else #f))))

; value-generic is a function to determine if an expression needs to be handled by value-boolean or value-int
(define value-generic
  (lambda (expression)
    (cond
      ((number? expression) (value-int expression))
      ((boolean? expression) (value-boolean expression))
      ((in-list? (operator expression) '(+ - * / %)) (value-int expression))
      ((in-list? (operator expression) '(== != > < <= >= && || !)) (value-boolean expression))
      (else (error ' bad-op "Invalid Operator")))))

; in-list takes an atom and a list and returns true if the element is in the list and false if the element is not in the list
(define in-list?
  (lambda (a lis)
    (cond
      ((null? lis)       #f)
      ((eq? a (car lis)) #t)
      (else             (in-list? a (cdr lis))))))

;abstractions
(define operator car)
(define first-operand cadr)
(define second-operand caddr)
(define third-element-null-check cddr)