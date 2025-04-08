#lang racket

(require "main.rkt" "parser/parser.rkt")
(require html-parsing)  ; raco pkg install html-parsing
(require ansi-color)    ; raco pkg install ansi-color
(provide
 test-html test-html-file test-html-single
 test-src
 test-raw-html
 parse-str)

;;;; CODE SOURCE FILE TESTER ;;;;
; Test all code files in the tests/src directory
(define (test-src [parser function-parser-str])
  (for ([filename (list-files "tests/src")])
    (display-test filename (parse filename parser))))


;;;; GENERATED HTML FILE TESTER ;;;;
; Evaluate the tests in the tests/html directory (generated from assignment tests by convert_tests.py)
; Usage: (test-html)
(define (test-html)
  (for ([filepath (list-files "tests/html")])
    (test-html-filepath filepath)))

; Evaluate the tests in the specified HTML file in the tests/html directory
; Usage: (test-html-file "part1tests")
(define (test-html-file filename)
  (test-html-filepath (~a "tests/html/" filename ".html")))

; Evaluate the specified test in the specified HTML file in the tests/html directory
; Do not catch errors (allows the error stack to be viewed)
; Usage: (test-html-single "part1tests" 5)
(define (test-html-single filename test-number [parser-str function-parser-str])
  (for ([test (get-tests-from-html (~a "tests/html/" filename ".html"))])
    (if (equal? (nt-field 'number test) test-number)
        (begin
          (display-test-header filename test)
          (foreground-color 'blue)
          (background-color 'black)
          (color-display (~a "\n" (nt-field 'code test)))
          (foreground-color 'white)
          (display "\n")
          (display (~a "Result: " (interpret-tree (parse-str (nt-field 'code test) parser-str)))))
        #f)))

; Run the provided test object from the provided file
; Return 0 if the test passes and 1 if it fails; print colorized results
(define (run-test filename test parser-str)
  (begin
    (display-test-header filename test)
    (if (evaluate-test filename test parser-str) 0 1)))

; Run the tests in the provided HTML file (generated from assignment tests by convert_tests.py)
(define (test-html-filepath filepath)
  (let ([parser-str (get-parser-version (html->xexp (open-input-file filepath)))])
    (begin
      (display "\n\n")
      (foreground-color 'black)
      (background-color 'white)
      (color-display (~a "\n\n##### Testing file '" filepath "' with " parser-str " parser #####\n"))
      (foreground-color 'white)
      (background-color 'black)
      (display "\n")
      (let
          ([numfailure (apply + (map (lambda (test) (run-test filepath test parser-str))
                                     (get-tests-from-html filepath)))])
        (begin (background-color (if (1 . <= . numfailure) 'red 'green))
               (display "\n")
               (color-display (~a "\nFailed " numfailure " tests in this file")))))))

; Print the metadata about test from file filename
(define (display-test-header filename test)
  (display (~a "\n" filename "\tTest " (nt-field 'number test) " (" (nt-field 'description test)
               ")\n" filename "\t\tExpect " (if (nt-field 'does-error test) "error" "return") ": "
               (nt-field 'result test))))

; Evaluate the given test from file filename
; Return #t if it passes or #f if it doesn't
; Display the result, color-coded to indicate success
(define (evaluate-test filename test parser-str)
  (begin
    (background-color 'black)
    (foreground-color 'white)
    (with-handlers ([exn:fail?
                     (lambda (v) (begin (foreground-color (if (nt-field 'does-error test) 'yellow 'red))
                                        (color-display (~a "\n" filename "\t\tGot error: " v))
                                        (nt-field 'does-error test)))])
      (letrec ([result (interpret-tree (parse-str (nt-field 'code test) parser-str))]
               [success (if (nt-field 'does-error test)
                            #f
                            (equal? (nt-field 'result test) result))])
        (begin (foreground-color (if success 'green 'red))
               (color-display (~a "\n" filename "\t\tGot return value: " result))
               success)))))

; Return list of tests in the given HTML file
(define (get-tests-from-html filepath)
  (get-tests (html->xexp (open-input-file filepath))))

; Return list of tests in the given HTML xexp object
(define (get-tests xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (append (get-tests (car xexp)) (get-tests (cdr xexp)))]
    [(equal? (car xexp) 'test) (list (parse-test (cdr xexp)))]
    [else (get-tests (cdr xexp))]))

; Return a "test" object representing the HTML test in the given HTML xexp object
; Test object is a list of pairs with the form:
; (('number int)
;  ('description str)
;  ('does-error bool)
;  ('result any)
;  ('code str))
(define (parse-test xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (cons (parse-test (car xexp)) (parse-test (cdr xexp)))]
    [(equal? (car xexp) 'number) (list 'number (string->number (cadr xexp)))]
    [(equal? (car xexp) 'description) xexp]
    [(equal? (car xexp) 'result)
     (cond
       [(equal? (cadr xexp) "true") '(result true)]
       [(equal? (cadr xexp) "false") '(result false)]
       [(string->number (cadr xexp)) (list 'result (string->number (cadr xexp)))]
       [else xexp])]
    [(equal? (car xexp) 'does-error) (list 'does-error (equal? (cadr xexp) "true"))]
    [(equal? (car xexp) 'code) (list 'code (join-xexp-strs (cdr xexp)))]
    [else (parse-test (cdr xexp))]))

; Get the value of the field named field-atom from the test object test
(define (nt-field field-atom test)
  (cond
    [(null? test) (error (~a "Failed to find field '" field-atom "' in test"))]
    [(and
      (list? (car test))
      (equal? (caar test) field-atom))
     (cadar test)]
    [else (nt-field field-atom (cdr test))]))

; Join a list of strings from an xexp into a single string,
;  removing elements that are just newlines
(define (join-xexp-strs xexp-strs)
  (string-join
   (filter (lambda (el) (not (or (equal? el "\r\n") (equal? el "\n")))) xexp-strs)
   ""))


;;;; OLD NON-NORMALIZED HTML TESTER ;;;;
; Test code from all <pre> tags in all HTML files in the tests/raw_html directory
(define (test-raw-html [parser-str function-parser-str])
  (for ([filename (list-files "tests/raw_html")])
    (for ([str (get-pre-contents-from-html filename)])
      (display-test str (parse-str str parser-str)))))

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
    [(equal? (car xexp) 'pre) (list (string-join (cdr xexp) ""))]
    [else (get-pre-contents (cdr xexp))]))


;;;; UTILITIES ;;;;
; Get a parse tree from a code string
; Uses temporary file tests/temp.j
(define (parse-str str [parser-str function-parser-str])
  (begin
    (with-output-to-file "tests/temp.j"
      (lambda () (printf str))
      #:exists 'replace)
    (let ([parsed (parse "tests/temp.j" parser-str)])
      (if (equal? parser-str simple-parser-str)
          parsed  ; TODO: Wrap in main() function
          parsed))))

; List the files in a directory
(define (list-files dir)
  (map
   (lambda (path) (~a dir "/" (path->string path)))
   (directory-list (~a dir "/"))))

; Get the head of an HTML file xexp
(define (get-head xexp)
  (car (cddadr xexp)))

; Get the string version of an HTML file xexp
(define (get-parser-version xexp)
  (with-handlers ([exn:fail? (lambda (v) (error "Failed to get parser version string from HTML"))])
    (cadar (cddddr (get-head xexp)))))