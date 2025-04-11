#lang racket

(require "simpleParser.rkt" "functionParser.rkt")

(provide parse
         simple-parser-str function-parser-str parser-str->func
         function-parser simple-parser)

; Parse filename using the parser indicated by parser-str (default function-parser-str)
; If parser-str is simple-parser-str, result will be wrapped in "function main () { ... }"
(define (parse filename [parser-str function-parser-str])
  ((parser-str->func parser-str) filename))

; Convert the parser-str into the corresponding parsing function
(define (parser-str->func parser-str)
  (if (equal? parser-str simple-parser-str) simple-parser function-parser))

; Parser strings to be passed to functions that can use different parsers
(define simple-parser-str "simple")
(define function-parser-str "function")