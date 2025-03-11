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
  (state-statement-list tree
                        empty-stt
                        (lambda (s) binding-uninit)
                        (lambda (v) v)
                        (lambda (s) (error "Break"))
                        (lambda (s) (error "Continue"))
                        (lambda (e s) (error (~a "Error: " e)))))

; Recursively returns the state after a series of statement lists
; Returns early if the return value in the state is set
(define (state-statement-list tree state next return break continue throw)
  (if (null? tree)
      (return binding-uninit)
      (state-generic (first-statement tree)
                     state
                     (lambda (s) (state-statement-list (next-statements tree) s next return break continue throw))
                     return
                     break
                     continue
                     throw)))

; Wrapper for returning state from different statement types
(define (state-generic expr state next return break continue throw)
  (let ([type (statement-type expr)]
        [body (statement-body expr)])
    (cond
      [(eq? type 'var)       (state-declare body state next)]
      [(eq? type '=)         (state-assign  body state next)]
      [(eq? type 'if)        (state-if      body state next return break continue throw)]
      [(eq? type 'while)     (state-while   body state next return (lambda (s) (next s)) continue throw)]
      [(eq? type 'return)    (return        (value-generic (return-value body) state))]
      [(eq? type 'break)     (break         state)]
      [(eq? type 'continue)  (continue      state)]
      [(eq? type 'throw)     (throw         (value-generic (thrown-value body) state) state)])))

; Returns state after a declaration
; Declaration statements may or may not contain an initialization value
(define (state-declare expr state next)
  (if (initializes? expr)
      (next (binding-create (variable expr) (value-generic (value expr) state) state))
      (next (binding-create (variable expr) binding-uninit state))))

; Returns state after an assignment
(define (state-assign expr state next)
  (next (binding-set (variable expr) (value-generic (value expr) state) state)))

; Returns state after an if statement
(define (state-if expr state next return break continue throw)
  (cond
    [(eq? 'true (value-generic (conditional-expr expr) state))
     (state-generic (then-expr expr) state next return break continue throw)]
    [(contains-else? expr)
     (state-generic (else-expr expr) state next return break continue throw)]
    [else (next state)]))

; Returns state after a while statement
(define (state-while expr state next return break continue throw)
  (if (eq? 'false (value-generic (conditional-expr expr) state))
      (next state)
      (state-generic (body-expr expr)
                     state
                     (lambda (s) (state-while expr s next return break continue throw))
                     return
                     break
                     (lambda (s) (state-while expr s next return break continue throw))
                     throw)))
   
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

; throw
(define thrown-value car)
