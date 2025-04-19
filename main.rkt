#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 4: OO Interpreter
;;;; ***************************************************

(require "parser/parser.rkt" "binding.rkt" "common.rkt")
(provide interpret interpret-tree)

; Takes a filename, calls specified parser with the filename, and returns the proper value
(define (interpret filename classname [parser-str class-parser-str])
  (interpret-tree (parse filename parser-str) classname))

; Takes a syntax tree in list format and returns the return value of the main method
; Error if tree contains syntax errors
(define (interpret-tree tree classname)
  (state-first-pass-list
   tree
   empty-stt
   (lambda (s) 
     (define class-closure (binding-lookup (string->symbol classname) s)) ;not sure if this actually works
     (cond
       [(not class-closure)
        (error (~a "Error: Class " classname " not found.") s)]
       [else
        (define method-closure (binding-lookup class-closure 'main)) ;not sure if this actually works
        (cond
          [(not method-closure)
           (error (~a "Error: Method main not found in class " classname) s)]
          [else
           (state-statement-list (closure-body method-closure)
                                 s
                                 (lambda (s) binding-uninit)
                                 identity
                                 (lambda (e s) (error (~a "Error: " e))))])]))
   (lambda (e s) (error (~a "Error in global pass: " e)))))

; Perform the first pass (global scope) of tree
; Call next on the resulting state
(define (state-first-pass-list tree state next throw)
  (if (null? tree)
    (next state)
    (state-first-pass-generic (first-statement tree)
                              state
                              (lambda (s) (state-first-pass-list (next-statements tree) s next throw))
                              throw)))

; Evaluate a statement for the first pass (global scope)
; Allows variable declaration/assignments & func declarations
; Call next on the resulting state or throw on any other statement type
(define (state-first-pass-generic expr state next throw)
  (let ([type (statement-type expr)]
        [body (statement-body expr)])
    (cond
      [(eq? type 'var)      (state-declare  body state next throw)]
      [(eq? type 'function) (state-func-dec body state next)]
      [else (throw (~a "Illegal " type " statement in top-level scope: '" expr "'") state)])))

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
      [(eq? type 'var)       (state-declare   body state next throw)]
      [(eq? type '=)         (state-assign    body state next throw)]
      [(eq? type 'if)        (state-if        body state next return break continue throw)]
      [(eq? type 'while)     (state-while     body state next return next continue throw)]
      [(eq? type 'return)    (value-generic   (return-value body) state return throw)]
      [(eq? type 'break)     (break           state)]
      [(eq? type 'continue)  (continue        state)]
      [(eq? type 'throw)     (value-generic   (thrown-value body) state (lambda (v) (throw v state)) throw)]
      [(eq? type 'try)       (state-try       body state next return break continue throw)]
      [(eq? type 'catch)     (state-catch     body state next return break continue throw)]
      [(eq? type 'finally)   (state-finally   body state next return break continue throw)]
      [(eq? type 'begin)     (state-block     body state next return break continue throw)]
      [(eq? type 'function)  (state-func-dec  body state next)]
      [(eq? type 'funcall)   (state-func-call body state next return throw)]
      [else                  (error (~a "Invalid syntax: " type))])))

; Returns state after running a block of statements
(define (state-block body state next return break continue throw)
  (state-statement-list body
                        (binding-push-layer state #f)
                        (lambda (s) (next (binding-pop-layer s)))
                        return
                        (lambda (s) (break (binding-pop-layer s)))
                        (lambda (s) (continue (binding-pop-layer s)))
                        (lambda (e s) (throw e (binding-pop-layer s)))))

; Returns state after a declaration
; Declaration statements may or may not contain an initialization value
(define (state-declare expr state next throw)
  (if (initializes? expr)
      (value-generic (value expr)
                     state
                     (lambda (v) (next (binding-create (variable expr) v state)))
                     throw)
      (next (binding-create (variable expr) binding-uninit state))))

; Returns state after an assignment
(define (state-assign expr state next throw)
  (value-generic (value expr) state (lambda (v) (next (binding-set (variable expr) v state))) throw))

; Returns state after an if statement
(define (state-if expr state next return break continue throw)
  (cond
    [(eq? 'true (value-generic (conditional-expr expr) state identity throw))
     (state-generic (then-expr expr) state next return break continue throw)]
    [(contains-else? expr)
     (state-generic (else-expr expr) state next return break continue throw)]
    [else (next state)]))

; Returns state after a while statement
(define (state-while expr state next return break continue throw)
  (if (eq? 'false (value-generic (conditional-expr expr) state identity throw))
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
  (let ([finally-cont (lambda (s) (state-generic (finally-block expr) s next return break continue throw))]
        [return-finally-cont (lambda (v) (state-generic (finally-block expr) state (lambda (s) (return v)) return break continue throw))])
    (cond
      [(and (contains-catch? expr) (contains-finally? expr))
       (state-block (try-body expr) state finally-cont return-finally-cont finally-cont finally-cont
                    (lambda (e s)
                      (state-generic (catch-block expr)
                                     (binding-create (caught-value expr) e s)
                                     finally-cont
                                     return-finally-cont
                                     finally-cont
                                     finally-cont
                                     (lambda (e s) (state-generic (finally-block expr) s next return break continue throw)))))]
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
       (state-block (try-body expr) state finally-cont return-finally-cont finally-cont finally-cont
                    (lambda (e s) (state-generic (finally-block expr) s (lambda (s) (throw e s)) return break continue
                                                 (lambda (e s) (state-generic (finally-block expr) s next return break continue throw)))))]
      [else (error "Try block must have at least one catch or finally block")])))


; Returns state after a catch block
(define (state-catch expr state next return break continue throw)
  (state-block (catch-body expr) state next return break continue throw))

; Returns state after a finally block
(define (state-finally expr state next return break continue throw)
  (state-block (finally-body expr) state next return break continue throw))

; Handles function definition by binding function name to closure
(define (state-func-dec func-dec state next)
  (let ([idx (binding-layer-idx state)])
    (next (binding-create (func-dec-name func-dec)
                          (list (func-dec-formal-params func-dec)
                                (func-dec-body func-dec)
                                (lambda (new-state)
                                  (binding-state-by-layer-idx new-state idx)))
                          state))))

; Handles function call that does not return a value
(define (state-func-call func-call state next return throw)
  ; Ignore values from return & next (this is a value func so next's parameter is a value, not a state!)
  (value-func-call func-call state (lambda (v) (next state)) (lambda (v) (next state)) throw))

; Bind actual parameters to formal parameters during a function call
(define (bind-params formal-params actual-params func-state curr-state throw)
  (if (null? actual-params)
      func-state
      (bind-params (next-params formal-params)
                   (next-params actual-params)
                   (value-generic (first-param actual-params)
                                  curr-state
                                  (lambda (v) (binding-create (first-param formal-params) v func-state))
                                  throw)
                   curr-state
                   throw)))


; ======================================================
; State helper functions

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


; ======================================================
; State Abstractions

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

; functions
(define func-dec-name car)
(define func-dec-formal-params cadr)
(define func-dec-body caddr)


;;;; ===================================================
;;;; Value
;;;; ===================================================


; get value of expression, assuming expression uses a binary operator
(define (value-binary-operator expression state next throw)
  (value-generic (first-operand-literal expression) state
                 (lambda (op1)
                   (value-generic (second-operand-literal expression) state
                                  (lambda (op2)
                                    (let ([op (operator expression)])
                                      (cond
                                        [(eq? '+  op) (next (op-plus   op1 op2))]
                                        [(eq? '-  op) (next (op-minus  op1 op2))]
                                        [(eq? '*  op) (next (op-times  op1 op2))]
                                        [(eq? '/  op) (next (op-divide op1 op2))]
                                        [(eq? '%  op) (next (op-modulo op1 op2))]
                                        [(eq? '== op) (next (cond-eq   op1 op2))]
                                        [(eq? '!= op) (next (cond-neq  op1 op2))]
                                        [(eq? '>  op) (next (cond-gt   op1 op2))]
                                        [(eq? '<  op) (next (cond-lt   op1 op2))]
                                        [(eq? '<= op) (next (cond-leq  op1 op2))]
                                        [(eq? '>= op) (next (cond-geq  op1 op2))]
                                        [(eq? '&& op) (next (bool-and  op1 op2 (lambda (e) (throw e state))))]
                                        [(eq? '|| op) (next (bool-or   op1 op2 (lambda (e) (throw e state))))]
                                        [else (throw (~a "Invalid binary operator: " op) state)]))) throw))
                 throw))

; get value of expression, assuming expression uses a unary operator
(define (value-unary-operator expression state next throw)
  (value-generic (first-operand-literal expression) state
                 (lambda (op1)
                   (let ([op (operator expression)])
                     (cond
                       [(eq? '- op)  (next (op-unary-minus op1))]
                       [(eq? '! op)  (next (bool-not       op1 (lambda (e) (throw e state))))]
                       [else (throw (~a "Invalid unary operator: " op) state)])))
                 throw))

; get value of a function call
(define (value-func-call func-call state return next throw)
  (let ([closure (binding-lookup (func-call-name func-call) state)])
    (if (not (same-length? (closure-formal-params closure) (func-call-actual-params func-call)))
        (throw (~a "Function called with wrong number of parameters. Expected "
                   (length (closure-formal-params closure)) ", got "
                   (length (func-call-actual-params func-call)) ".") state)
        (state-statement-list (closure-body closure)
                              (bind-params (closure-formal-params closure)
                                           (func-call-actual-params func-call)
                                           (binding-push-layer ((closure-scope-func closure) state) #t)
                                           state
                                           throw)
                              return
                              next
                              (lambda (s) (throw (~a "Break outside of loop in function " (func-call-name func-call)) state))
                              (lambda (s) (throw (~a "Continue outside of loop in function " (func-call-name func-call)) state))
                              (lambda (e s) (throw e state))))))

; get the value of expression, regardless of type or operator aryness
(define (value-generic expression state next throw)
  (cond
    [(number? expression) (next expression)]
    [(boolean-literal? expression) (next expression)]
    [(eq? (binding-status expression state) binding-init) (next (binding-lookup expression state))]
    [(eq? (binding-status expression state) binding-uninit) (throw (~a expression " has not been assigned a value") state)]
    [(not (pair? expression)) (throw (~a expression " has not been declared") state)]
    [(eq? (expr-start expression) 'funcall)
     (value-func-call (expr-func-call expression)
                      state
                      (lambda (v) (throw (~a "No return statement in function " (func-call-name (expr-func-call expression))) state))
                      next
                      throw)]
    [(has-second-operand? expression) (value-binary-operator expression state next throw)]
    [(has-first-operand? expression) (value-unary-operator expression state next throw)]
    [else (throw (~a "Invalid operator: " (operator expression)) state)]))

; ======================================================
; Operations

; Create binary boolean condition function that returns atom 'true/'false from racket function that
;  returns #t/#f
(define (build-condition racket-op)
  (lambda (op1 op2)
    (if (racket-op op1 op2)
        'true
        'false)))

; Create function that runs racket-op on two inputs but errors if either input is not an integer
; op-atom is an atom that describes the operation for use in error printing
(define (typesafe-binary-int-op racket-op op-atom)
  (typesafe-binary-op racket-op op-atom integer? 'integer))

; Create function that runs racket-op on one input but errors if the input is not an integer
; op-atom is an atom that describes the operation for use in error printing
(define (typesafe-unary-int-op racket-op op-atom)
  (typesafe-unary-op racket-op op-atom integer? 'integer))

; Create function that runs racket-op on two inputs but errors if either input fails predicate
; op-atom & predicate-atom are atoms that describe the operation & predicate for use in error printing
(define (typesafe-binary-op racket-op op-atom predicate predicate-atom)
  (lambda (op1 op2)
    (cond
      [(not (predicate op1)) (error (~a "Invalid left hand side of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [(not (predicate op2)) (error (~a "Invalid right hand side of operator " op-atom ": expected " predicate-atom ", got " op2))]
      [else (racket-op op1 op2)])))

; Create function that runs racket-op on one input but errors if the input fails predicate
; op-atom & predicate-atom are atoms that describe the operation & predicate for use in error printing
(define (typesafe-unary-op racket-op op-atom predicate predicate-atom)
  (lambda (op1)
    (cond
      [(not (predicate op1)) (error (~a "Invalid operand of operator " op-atom ": expected " predicate-atom ", got " op1))]
      [else (racket-op op1)])))

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
(define (bool-not op1 throw)
  (cond
    [(eq? op1 'true) 'false]
    [(eq? op1 'false) 'true]
    [else (throw "Inversion can only be applied to booleans")]))

; boolean and using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-and op1 op2 throw)
  (cond
    [(eq?        op1 'false)  'false]
    [(not-equal? op1 'true) (throw (~a "And can only be applied to booleans, got " op1))]
    [(eq?        op2 'false)  'false]
    [(not-equal? op2 'true) (throw (~a "And can only be applied to booleans, got " op2))]
    [else 'true]))

; boolean or using 'true/'false atoms with explicit short circuiting
; errors on operands that aren't boolean atoms
(define (bool-or op1 op2 throw)
  (cond
    [(eq?        op1 'true)  'true]
    [(not-equal? op1 'false) (throw (~a "Or can only be applied to booleans, got " op1))]
    [(eq?        op2 'true)  'true]
    [(not-equal? op2 'false) (throw (~a "Or can only be applied to booleans, got " op2))]
    [else 'false]))

; ======================================================
; Value Abstractions

; return true if expression is a boolean atom ('true or 'false)
(define (boolean-literal? expression) (or (eq? expression 'true) (eq? expression 'false)))

(define (has-second-operand? expression) (not-null? (cddr expression)))
(define (has-first-operand? expression) (not-null? (cdr expression)))

(define operator car)
(define first-operand-literal cadr)
(define second-operand-literal caddr)

(define first-param car)
(define next-params cdr)
(define expr-start car)
(define expr-func-call cdr)
(define func-call-name car)
(define func-call-actual-params cdr)
(define closure-formal-params car)
(define closure-body cadr)
(define closure-scope-func caddr)