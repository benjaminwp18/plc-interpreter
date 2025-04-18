#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 4: OO Interpreter
;;;; ***************************************************

(provide not-null? in-list? not-equal? same-length?)

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

(define (same-length? lis1 lis2)
  (cond
    [((null? lis1) . and . (null? lis2)) #t]
    [((null? lis1) . or . (null? lis2)) #f]
    [else (same-length? (cdr lis1) (cdr lis2))]))