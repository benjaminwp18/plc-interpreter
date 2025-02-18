#lang racket

(require "state.rkt")
(require "parser/simpleParser.rkt")

(require html-parsing)

(provide (all-defined-out))

(define (test-src)
  (map
   (lambda (str) (display (~a "Testing " str "...\n"
                              (with-handlers ([exn:fail? (lambda (v) (~a v "\n"))])
                                (interpret (parser str))) "\n")))
   (list-files "tests/src")))

(define (test-html)
  (map
   (lambda (filename)
     (map
      (lambda (str) (display (~a "Testing " str "...\n"
                                 (with-handlers ([exn:fail? (lambda (v) (~a v "\n"))])
                                   (interpret (parse-str str))) "\n\n")))
      (parse-pres filename)))
   (list-files "tests/html")))

(define (parse-pres filename)
  (find-pres (html->xexp (open-input-file filename))))

(define (find-pres xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (append (find-pres (car xexp)) (find-pres (cdr xexp)))]
    [(eq? (car xexp) 'pre) (list (string-join (cdr xexp) ""))]
    [else (find-pres (cdr xexp))]))

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

