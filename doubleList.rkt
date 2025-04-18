#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 4: OO Interpreter
;;;; ***************************************************

(provide dl-lookup dl-create dl-unbound dl-unbound empty-dl dl-empty?)

; Return bound value of name in double-list
; Error if binding does not exist
(define (dl-lookup name double-list)
  (cond
    [(dl-empty? double-list) dl-unbound]
    [(eq? (dl-first-name double-list) name) (dl-first-val double-list)]
    [else (dl-lookup name (dl-cdr double-list))]))

(define dl-unbound '())

; Return double-list with new binding (name, value)
; Error if binding already exists
(define (dl-create name value double-list)
  (cond
    [(dl-empty? double-list) (dl-cons (list name value) empty-dl)]
    [(eq? (dl-first-name double-list) name) (error (~a "Binding for " name " already exists in double list"))]
    [else (dl-cons (dl-car double-list) (dl-create name value (dl-cdr double-list)))]))

(define empty-dl '(() ()))
(define (dl-empty? double-list)
  (or (null? (dl-entry-names double-list))
      (null? (dl-entry-vals double-list))))

(define dl-entry-names car)
(define dl-entry-vals cadr)
(define dl-entry-name car)
(define dl-entry-val cadr)
(define (dl-first-name double-list)
  (car (dl-entry-names double-list)))
(define (dl-first-val double-list)
  (car (dl-entry-vals double-list)))
(define (dl-car double-list)
  (list
   (car (dl-entry-names double-list))
   (car (dl-entry-vals double-list))))
(define (dl-cdr double-list)
  (list
   (cdr (dl-entry-names double-list))
   (cdr (dl-entry-vals double-list))))
(define (dl-cons pair double-list)
  (list
   (cons (dl-entry-name pair) (dl-entry-names double-list))
   (cons (dl-entry-val pair) (dl-entry-vals double-list))))