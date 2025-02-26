#lang racket

(require "state.rkt" "parser/simpleParser.rkt")
(require html-parsing)
(provide test-src test-html parse-str)

; Test all code files in the tests/src directory
(define (test-src)
  (for ([filename (list-files "tests/src")])
    (display-test filename (parser filename))))

; Test code from all <pre> tags in all HTML files in the tests/html directory
(define (test-html)
  (for ([filename (list-files "tests/html")])
    (for ([str (get-pre-contents-from-html filename)])
      (display-test str (parse-str str)))))

; Display the results of a test
; str is some identifier for the test
; parsetree is the parse tree of the code to test
(define (display-test str parsetree)
  (display (~a "======\n" str "\n---\nReturn value: "
               (with-handlers ([exn:fail? (lambda (v) (~a v "\n"))])
                 (interpret-tree parsetree))
               "\n======\n\n")))

; Convert an HTML file to a list of strings
; s.t. each string is the contents of a <pre> tag
(define (get-pre-contents-from-html filename)
  (get-pre-contents (html->xexp (open-input-file filename))))

; Convert a xexp list to a list of strings
; s.t. each string is the contents of a '(pre (...)) tag
(define (get-pre-contents xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (append (get-pre-contents (car xexp)) (get-pre-contents (cdr xexp)))]
    [(eq? (car xexp) 'pre) (list (string-join (cdr xexp) ""))]
    [else (get-pre-contents (cdr xexp))]))

; Get a parse tree from a code string
; Uses temporary file tests/temp.j
(define (parse-str str)
  (begin
    (with-output-to-file "tests/temp.j"
      (lambda () (printf str))
      #:exists 'replace)
    (parser "tests/temp.j")))

; List the files in a directory
(define (list-files dir)
  (map
   (lambda (path) (~a dir "/" (path->string path)))
   (directory-list (~a dir "/"))))

