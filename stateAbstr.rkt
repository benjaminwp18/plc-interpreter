#lang racket
(provide (all-defined-out))

(define (lookup-binding name state)
  (cond
    [(stt-empty? state) '()]
    [(eq? (stt-first-name state) name) (stt-first-val state)]
    [else (lookup-binding name (stt-cdr state))]))

(define (bound? name state)
  (not-null? (lookup-binding name state)))

(define (set-binding name value state)
  (cond
    [(stt-empty? state) (error (~a "Binding for " name " does not exist"))]
    [(eq? (stt-first-name state) name) (stt-cons (list name value) (stt-cdr state))]
    [else (stt-cons (stt-car state) (set-binding name value (stt-cdr state)))]))

(define (create-binding name value state)
  (cond
    [(stt-empty? state) (stt-cons (list name value) empty-stt)]
    [(eq? (stt-first-name state) name) (error (~a "Binding for " name " already exists"))]
    [else (stt-cons (stt-car state) (create-binding name value (stt-cdr state)))]))

(define (not-null? a) (not (null? a)))

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
(define empty-stt '(() ()))
(define (stt-empty? state)
  (or (null? (stt-names state))
      (null? (stt-vals state))))

  