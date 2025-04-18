#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 4: OO Interpreter
;;;; ***************************************************

(require "simpleParser.rkt" "functionParser.rkt" "classParser.rkt")

(provide parse
         simple-parser-str function-parser-str class-parser-str parser-str->func
         function-parser simple-parser)

; Parse filename using the parser indicated by parser-str (default function-parser-str)
; If parser-str is simple-parser-str, result will be wrapped in "function main () { ... }"
(define (parse filename [parser-str class-parser-str])
  ((parser-str->func parser-str) filename))

; Convert the parser-str into the corresponding parsing function
(define (parser-str->func parser-str)
  (cond
    [(equal? parser-str class-parser-str) class-parser]
    [(equal? parser-str function-parser-str) function-parser]
    [else simple-parser]))

; Parser strings to be passed to functions that can use different parsers
(define simple-parser-str "simple")
(define function-parser-str "function")
(define class-parser-str "class")