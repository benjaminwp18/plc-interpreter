#lang racket

(provide binding-lookup binding-status binding-set binding-create binding-unbound binding-uninit binding-init empty-stt stt-empty?)

; Return bound value of name in state
; Error if binding does not exist
(define (binding-lookup name state)
  (cond
    [(stt-empty? state) (error (~a name " has not been declared"))]
    [(eq? (stt-first-name state) name) (stt-first-val state)]
    [else (binding-lookup name (stt-cdr state))]))

; Return binding-unbound if name's binding does not exist in state
; Return binding-uninit if it's not initialized
; Return binding-init otherwise
(define (binding-status name state)
  (with-handlers ([exn:fail? (lambda (v) binding-unbound)])
    (if (eq? (binding-lookup name state) binding-uninit)
        binding-uninit
        binding-init)))

(define binding-unbound "unbound")
(define binding-uninit '())
(define binding-init "initialized")

; Return state with value set for name's binding
; Error if binding does not exist
(define (binding-set name value state)
  (cond
    [(stt-empty? state) (error (~a name " has not been declared"))]
    [(eq? (stt-first-name state) name) (stt-cons (list name value) (stt-cdr state))]
    [else (stt-cons (stt-car state) (binding-set name value (stt-cdr state)))]))

; Return state with new binding (name, value)
; Error if binding already exists
(define (binding-create name value state)
  (cond
    [(stt-empty? state) (stt-cons (list name value) empty-stt)]
    [(eq? (stt-first-name state) name) (error (~a "Binding for " name " already exists"))]
    [else (stt-cons (stt-car state) (binding-create name value (stt-cdr state)))]))

(define empty-stt '(() ()))
(define (stt-empty? state)
  (or (null? (stt-names state))
      (null? (stt-vals state))))

(define stt-names car)
(define stt-vals cadr)
(define stt-name car)
(define stt-val cadr)
(define (stt-first-name state)
  (car (stt-names state)))
(define (stt-first-val state)
  (car (stt-vals state)))
(define (stt-car state)
  (list
   (car (stt-names state))
   (car (stt-vals state))))
(define (stt-cdr state)
  (list
   (cdr (stt-names state))
   (cdr (stt-vals state))))
(define (stt-cons pair state)
  (list
   (cons (stt-name pair) (stt-names state))
   (cons (stt-val pair) (stt-vals state))))