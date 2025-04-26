#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 4: OO Interpreter
;;;; ***************************************************

(provide dl-lookup dl-get-reverse-index dl-cons
         dl-unbound empty-dl dl-empty? dl-names dl-box-vals)

; Return bound value of name in double-list
; Return dl-unbound if it isn't found
(define (dl-lookup name double-list [boxed #f])
  (cond
    [(dl-empty? double-list) dl-unbound]
    [(equal? (dl-first-name double-list) name) (unbox (dl-first-val double-list))]
    [else (dl-lookup name (dl-cdr double-list))]))

(define (dl-lookup-boxed name double-list)
  (dl-lookup name double-list #t))

(define (dl-box-vals double-list)
  (map box (dl-vals double-list)))

(define (dl-get-reverse-index name double-list)
  (cond
    [(dl-empty? double-list) dl-unbound]
    [(eq? (dl-first-name double-list) name) (sub1 (dl-length double-list))]
    [else (dl-get-reverse-index name (dl-cdr double-list))]))

(define (dl-length double-list)
  (length (dl-vals double-list)))

(define dl-unbound '())

(define empty-dl '(() ()))
(define (dl-empty? double-list)
  (or (null? (dl-names double-list))
      (null? (dl-vals double-list))))

(define dl-names car)
(define dl-vals cadr)
(define dl-entry-name car)
(define dl-entry-val cadr)
(define (dl-first-name double-list)
  (car (dl-names double-list)))
(define (dl-first-val double-list)
  (car (dl-vals double-list)))
(define (dl-car double-list)
  (list
   (car (dl-names double-list))
   (car (dl-vals double-list))))
(define (dl-cdr double-list)
  (list
   (cdr (dl-names double-list))
   (cdr (dl-vals double-list))))
(define (dl-cons pair double-list)
  (list
   (cons (dl-entry-name pair) (dl-names double-list))
   (cons (dl-entry-val pair) (dl-vals double-list))))