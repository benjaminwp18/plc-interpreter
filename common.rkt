#lang racket
(provide not-null?)

(define (not-null? a) (not (null? a)))