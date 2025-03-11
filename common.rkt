#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 2: Flow Control Interpreter
;;;; ***************************************************

(provide not-null? in-list? not-equal?)

; returns true if a is not null; false otherwise
(define (not-null? a) (not (null? a)))

; returns true if a is in lis; false otherwise
(define (in-list? a lis)
    (cond
      [(null? lis)       #f]
      [(eq? a (car lis)) #t]
      [else             (in-list? a (cdr lis))]))

; returns true if a does not equal b
(define (not-equal? a b)
  (not (equal? a b)))