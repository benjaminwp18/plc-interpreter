#lang racket

(require "state.rkt")
(require "parser/simpleParser.rkt")

(require html-parsing)

(define (test-src)
  (map
   (lambda (str) (display (~a "Testing " str "...\n"
                              (with-handlers ([exn:fail? (lambda (v) (~a v "\n"))])
                                (interpret (parser str)) "\n"))))
   (list-files "tests/src")))

(define (test-html)
  (map
   (lambda (str) (display (~a "Testing " str "...\n" (interpret (parser str)) "\n")))
   (list-files "tests/html")))

(define (parse-pres filename)
  (find-pres (html->xexp (open-input-file filename))))

(define (find-pres xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) ]
    [(eq? (car xexp) 'pre)]))

(define (parse-str str)
  (begin
    (with-output-to-file "tests/temp.j"
      (lambda () (printf str))
      #:exists 'replace)
    (parser "tests/temp.j")))

(define (list-files dir)
   (map
    (lambda (path) (~a dir "/" (path->string path)))
    (directory-list (~a dir "/"))))

