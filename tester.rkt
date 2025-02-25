#lang racket

(require "state.rkt")
(require "parser/simpleParser.rkt")

(require html-parsing)

(provide test-src test-html)

(define (test-src)
  (for ([filename (list-files "tests/src")])
    (display-test filename (parser filename))))

(define (test-html)
  (for ([filename (list-files "tests/html")])
    (for ([str (parse-pres filename)])
      (display-test str (parse-str str)))))

(define (display-test str parsetree)
  (display (~a "======\n" str "\n---\nReturn value: "
               (with-handlers ([exn:fail? (lambda (v) (~a v "\n"))])
                 (interpret-tree parsetree))
               "\n======\n\n")))

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

