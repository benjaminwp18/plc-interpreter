#lang racket

(require "binding.rkt")
(require "value.rkt")
(require "common.rkt")

; TODO: value_generic state

(define interpret
  (lambda (tree)
    (binding-lookup 'return (state-statement-list tree (binding-create 'return binding-uninit empty-stt)))))

(define state-statement-list
  (lambda (tree state)
    (if (null? tree)
        state
        (state-statement-list (next-statements tree) (state-generic (first-statement tree) state)))))

(define state-generic
  (lambda (expr state)
    (cond
      [(eq? (statement-type expr) 'var)    (state-declare (statement-body expr) state)]
      [(eq? (statement-type expr) '=)      (state-assign  (statement-body expr) state)]
      [(eq? (statement-type expr) 'if)     (state-if      (statement-body expr) state)]
      [(eq? (statement-type expr) 'while)  (state-while   (statement-body expr) state)]
      [(eq? (statement-type expr) 'return) (state-return  (statement-body expr) state)])))

(define state-declare
  (lambda (expr state)
    (if (initializes? expr)
        (binding-create (variable expr) (value-generic (value expr)) state)
        (binding-create (variable expr) binding-uninit state))))

(define state-assign
  (lambda (expr state)
    (binding-set (variable expr) (value-generic (value expr)) state)))

(define state-if
  (lambda (expr state)
    (cond
      [(value-generic (conditional-expr expr)) (state-generic (then-expr expr) state)]
      [(contains-else? expr)                   (state-generic (else-expr expr) state)]
      [else                                    state])))

(define state-while
  (lambda (expr state)
    (if (eq? #f (value-generic (conditional-expr expr)))
      state
      (state-while expr (state-generic (body-expr expr) state)))))

(define state-return
  (lambda (expr state)
    (binding-set 'return (value-generic (return-value expr)) state)))

; ====================================
; Helper functions

(define initializes?
  (lambda (expr)
    (not-null? (value-pos expr))))

(define contains-else?
  (lambda (expr)
    (not-null? (else-expr-pos expr))))

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
