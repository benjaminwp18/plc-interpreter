#lang racket

(require "stateAbstr.rkt")
(require "kylecode.rkt")

; TODO: value_generic state

(define interpret
  (lambda (tree)
    (lookup-binding 'return (M_statement-list tree (create-binding 'return uninitialized empty-stt)))))

(define M_statement-list
  (lambda (tree state)
    (if (null? tree)
        state
        (M_statement-list (next-statements tree) (M_state (first-statement tree) state)))))

(define M_state
  (lambda (expr state)
    (cond
      ((eq? (statement-type expr) 'var)    (M_declare (statement-body expr) state))
      ((eq? (statement-type expr) '=)      (M_assign  (statement-body expr) state))
      ((eq? (statement-type expr) 'if)     (M_if      (statement-body expr) state))
      ((eq? (statement-type expr) 'while)  (M_while   (statement-body expr) state))
      ((eq? (statement-type expr) 'return) (M_return  (statement-body expr) state)))))

(define M_declare
  (lambda (expr state)
    (if (initializes? expr)
        (create-binding (variable expr) (value-generic (value expr)) state)
        (create-binding (variable expr) uninitialized state))))

(define M_assign
  (lambda (expr state)
    (set-binding (variable expr) (value-generic (value expr)) state)))

(define M_if
  (lambda (expr state)
    (cond
      ((value-generic (conditional-expr expr)) (M_state (then-expr expr) state))
      ((contains-else? expr)                   (M_state (else-expr expr) state))
      (else                                    state))))

(define M_while
  (lambda (expr state)
    (if (eq? #f (value-generic (conditional-expr expr)))
      state
      (M_while expr (M_state (body-expr expr) state)))))

(define M_return
  (lambda (expr state)
    (set-binding 'return (value-generic (return-value expr)) state)))

; ====================================
; Helper functions

(define initializes?
  (lambda (expr)
    (not (null? (value-pos expr)))))

(define contains-else?
  (lambda (expr)
    (not-null? (else-expr-pos expr))))

(define uninitialized '())

; ====================================
; Abstractions

; statement-list
(define first-statement car)
(define next-statements cdr)

; state
(define statement-type car)
(define statement-body cdr)

; declare and assign
(define variable car)
(define value-pos cdr)
(define value cadr)

; if and while
(define conditional-expr car)
(define then-expr cadr)
(define else-expr-pos cddr)
(define else-expr caddr)
(define body-expr cadr)

; return
(define return-value car)
