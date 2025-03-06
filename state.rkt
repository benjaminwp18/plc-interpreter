#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 1: Simple Language Interpreter
;;;; ***************************************************

(require "parser/simpleParser.rkt" "binding.rkt" "value.rkt" "common.rkt")
(provide interpret interpret-tree)

; Takes a filename, calls parser with the filename, and returns the proper value
(define (interpret filename)
  (interpret-tree (parser filename)))

; Takes a syntax tree in list format and returns its return value
; Error if tree contains syntax errors
(define (interpret-tree tree)
  (binding-lookup 'return (state-statement-list tree (binding-create 'return binding-uninit empty-stt))))

; Recursively returns the state after a series of statement lists
; Returns early if the return value in the state is set
(define (state-statement-list tree state)
  (if (or (null? tree) (not-null? (binding-lookup 'return state)))
      state
      (state-statement-list (next-statements tree) (state-generic (first-statement tree) state))))

; Wrapper for returning state from different statement types
(define (state-generic expr state)
  (let ([type (statement-type expr)]
        [body (statement-body expr)])
    (cond
      [(eq? type 'var)    (state-declare body state)]
      [(eq? type '=)      (state-assign  body state)]
      [(eq? type 'if)     (state-if      body state)]
      [(eq? type 'while)  (state-while   body state)]
      [(eq? type 'return) (state-return  body state)])))

; Returns state after a declaration
; Declaration statements may or may not contain an initialization value
(define (state-declare expr state)
  (if (initializes? expr)
      (binding-create (variable expr) (value-generic (value expr) state) state)
      (binding-create (variable expr) binding-uninit state)))

; Returns state after an assignment
(define (state-assign expr state)
  (binding-set (variable expr) (value-generic (value expr) state) state))

; Returns state after an if statement
(define (state-if expr state)
  (cond
    [(eq? (value-generic (conditional-expr expr) state) 'true) (state-generic (then-expr expr) state)]
    [(contains-else? expr) (state-generic (else-expr expr) state)]
    [else state]))

; Returns state after a while statement
(define (state-while expr state)
  (if (eq? 'false (value-generic (conditional-expr expr) state))
      state
      (state-while expr (state-generic (body-expr expr) state))))

; Sets return value in state after a return statement
(define (state-return expr state)
  (binding-set 'return (value-generic (return-value expr) state) state))

; ====================================
; Helper functions

; whether a declaration statement initializes the variable
(define (initializes? expr)
  (not-null? (value-pos expr)))

; whether an if statement contains an "else" expression
(define (contains-else? expr)
  (not-null? (else-expr-pos expr)))

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
