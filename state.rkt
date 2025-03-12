#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 2: Flow Control Interpreter
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
                        identity
                        (lambda (s) (error "'break' called outside loop"))
                        (lambda (s) (error "'continue' called outside loop"))
                        (lambda (e s) (error (~a "Error: " e)))))

; Returns the return value after recursing through a series of statement lists
(define (state-statement-list tree state next return break continue throw)
  (if (null? tree)
      (next state)
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
      [(eq? type 'while)     (state-while   body state next return next continue throw)]
      [(eq? type 'return)    (value-generic (return-value body) state return)]
      [(eq? type 'break)     (break         state)]
      [(eq? type 'continue)  (continue      state)]
      [(eq? type 'throw)     (value-generic (thrown-value body) state (lambda (v) (throw v state)))]
      [(eq? type 'try)       (state-try     body state next return break continue throw)]
      [(eq? type 'catch)     (state-catch   body state next return break continue throw)]
      [(eq? type 'finally)   (state-finally body state next return break continue throw)]
      [(eq? type 'begin)     (state-block   body state next return break continue throw)]
      [else                  (error (~a "Invalid syntax: " type))])))

; Returns state after running a block of statements
(define (state-block body state next return break continue throw)
  (state-statement-list body
                        (binding-push-layer state)
                        (lambda (s) (next (binding-pop-layer s)))
                        return
                        (lambda (s) (break (binding-pop-layer s)))
                        (lambda (s) (continue (binding-pop-layer s)))
                        (lambda (e s) (throw e (binding-pop-layer s)))))

; Returns state after a declaration
; Declaration statements may or may not contain an initialization value
(define (state-declare expr state next)
  (if (initializes? expr)
      (next (binding-create (variable expr) (value-generic (value expr) state identity) state))
      (next (binding-create (variable expr) binding-uninit state))))

; Returns state after an assignment
(define (state-assign expr state next)
  (next (binding-set (variable expr) (value-generic (value expr) state identity) state)))

; Returns state after an if statement
(define (state-if expr state next return break continue throw)
  (cond
    [(eq? 'true (value-generic (conditional-expr expr) state identity))
     (state-generic (then-expr expr) state next return break continue throw)]
    [(contains-else? expr)
     (state-generic (else-expr expr) state next return break continue throw)]
    [else (next state)]))

; Returns state after a while statement
(define (state-while expr state next return break continue throw)
  (if (eq? 'false (value-generic (conditional-expr expr) state identity))
      (next state)
      (state-generic (body-expr expr)
                     state
                     (lambda (s) (state-while expr s next return break continue throw))
                     return
                     break
                     (lambda (s) (state-while expr s next return break continue throw))
                     throw)))

; Returns state after a try block (catch or finally block may be empty)
(define (state-try expr state next return break continue throw)
  (let ([finally-continuation (lambda (s) (state-generic (finally-block expr) s next return break continue throw))])
    (cond
      [(and (contains-catch? expr) (contains-finally? expr))
       (state-block (try-body expr) state finally-continuation return finally-continuation finally-continuation
                    (lambda (e s)
                      (state-generic (catch-block expr)
                                     (binding-create (caught-value expr) e s)
                                     finally-continuation
                                     return
                                     finally-continuation
                                     finally-continuation
                                     (lambda (e s) (state-generic (finally-block expr) s next return break continue
                                                                  (lambda (e s) (state-generic (finally-block expr) s next return break continue throw)))))))]
      [(contains-catch? expr)
       (state-block (try-body expr) state next return break continue
                    (lambda (e s)
                      (state-generic (catch-block expr)
                                     (binding-create (caught-value expr) e s)
                                     next
                                     return
                                     break
                                     continue
                                     throw)))]
      [(contains-finally? expr)
       (state-block (try-body expr) state finally-continuation return finally-continuation finally-continuation
                    (lambda (e s) (state-generic (finally-block expr) s (lambda (s) (throw e s)) return break continue
                                                 (lambda (e s) (state-generic (finally-block expr) s next return break continue throw)))))]
      [else (error "Try block must have at least one catch or finally block")])))


; Returns state after a catch block
(define (state-catch expr state next return break continue throw)
  (state-block (catch-body expr) state next return break continue throw))

; Returns state after a finally block
(define (state-finally expr state next return break continue throw)
  (state-block (finally-body expr) state next return break continue throw))

   
; ====================================
; Helper functions

; whether a declaration statement initializes the variable
(define (initializes? expr)
  (not-null? (value-pos expr)))

; whether an if statement contains an else expression
(define (contains-else? expr)
  (not-null? (else-expr-pos expr)))

; whether a try block contains a catch block
(define (contains-catch? expr)
  (not-null? (catch-block expr)))

; whether a try block contains a finally block
(define (contains-finally? expr)
  (not-null? (finally-block expr)))

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

; try/catch
(define try-body car)
(define catch-block cadr)
(define finally-block caddr)
(define (caught-value expr) (caadr (catch-block expr)))
(define catch-body cadr)
(define finally-body car)