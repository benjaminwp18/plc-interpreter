#lang racket

(provide not-null? in-list?)

(define (not-null? a) (not (null? a)))

; in-list takes an atom and a list and returns true if the element is in the list and false if the element is not in the list
(define (in-list? a lis)
    (cond
      [(null? lis)       #f]
      [(eq? a (car lis)) #t]
      [else             (in-list? a (cdr lis))]))