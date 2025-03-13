#lang racket

(require "state.rkt" "parser/simpleParser.rkt")
(require html-parsing)  ; raco pkg install html-parsing
(require ansi-color)    ; raco pkg install ansi-color
(provide test-src test-raw-html test-norm-html parse-str)

; Test all code files in the tests/src directory
(define (test-src)
  (for ([filename (list-files "tests/src")])
    (display-test filename (parser filename))))

; Test code from all <pre> tags in all HTML files in the tests/html directory
(define (test-raw-html)
  (for ([filename (list-files "tests/html")])
    (for ([str (get-pre-contents-from-html filename)])
      (display-test str (parse-str str)))))

(define (test-norm-html)
  (for ([filename (list-files "tests/normalized_html")])
    (begin
      (display "\n\n")
      (foreground-color 'black)
      (background-color 'white)
      (color-display (~a "\n\n##### Testing file '" filename "' #####\n"))
      (foreground-color 'white)
      (background-color 'black)
      (display "\n")
      (let ([numfailure (apply + (map (lambda (test)
                                        (begin
                                          (display-test-header filename test)
                                          (if (evaluate-test filename test) 0 1)))
                                      (get-tests-from-norm-html filename)))])
           (begin (background-color (if (1 . <= . numfailure) 'red 'green))
                  (display "\n")
                  (color-display (~a "\nFailed " numfailure " tests in this file")))))))

(define (display-test-header filename test)
  (display (~a "\n" filename "\tTest " (nt-field 'number test) " (" (nt-field 'description test) ")\n"
              filename "\t\tExpect " (if (nt-field 'does-error test) "error" "return") ": " (nt-field 'result test))))

(define (evaluate-test filename test)
  (begin
    (background-color 'black)
    (foreground-color 'white)
    (with-handlers ([exn:fail?
                    (lambda (v) (begin (foreground-color (if (nt-field 'does-error test) 'yellow 'red))
                                       (color-display (~a "\n" filename "\t\tGot error: " v))
                                       (nt-field 'does-error test)))])
      (letrec ([result (interpret-tree (parse-str (nt-field 'code test)))]
               [success (if (nt-field 'does-error test)
                         #f
                         (equal? (nt-field 'result test) result))])
          (begin (foreground-color (if success 'green 'red))
                 (color-display (~a "\n" filename "\t\tGot return value: " result))
                 success)))))

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

(define (get-tests-from-norm-html filename)
  (get-tests (html->xexp (open-input-file filename))))

(define (get-tests xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (append (get-tests (car xexp)) (get-tests (cdr xexp)))]
    [(equal? (car xexp) 'test) (list (parse-test (cdr xexp)))]
    [else (get-tests (cdr xexp))]))

; (number description does-error result code)
(define (parse-test xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (cons (parse-test (car xexp)) (parse-test (cdr xexp)))]
    [(equal? (car xexp) 'number) xexp]
    [(equal? (car xexp) 'description) xexp]
    [(equal? (car xexp) 'result)
     (cond
      [(equal? (cadr xexp) "true") (list 'result 'true)]
      [(equal? (cadr xexp) "false") (list 'result 'false)]
      [(string->number (cadr xexp)) (list 'result (string->number (cadr xexp)))]
      [else xexp])]
    [(equal? (car xexp) 'does-error) (list 'does-error (equal? (cadr xexp) "true"))]
    [(equal? (car xexp) 'code) (list 'code (join-xexp-strs (cdr xexp)))]
    [else (parse-test (cdr xexp))]))

(define (nt-field field-atom norm-test)
  (cond
    [(null? norm-test) (error (~a "Failed to find field '" field-atom "' in test"))]
    [(and
      (list? (car norm-test))
      (equal? (caar norm-test) field-atom))
      (cadar norm-test)]
    [else (nt-field field-atom (cdr norm-test))]))

(define (join-xexp-strs xexp-strs)
  (string-join
    (filter (lambda (el) (not (or (equal? el "\r\n") (equal? el "\n")))) xexp-strs)
    ""))

; Convert a xexp list to a list of strings
; s.t. each string is the contents of a '(pre (...)) tag
(define (get-pre-contents xexp)
  (cond
    [(null? xexp) '()]
    [(list? (car xexp)) (append (get-pre-contents (car xexp)) (get-pre-contents (cdr xexp)))]
    [(equal? (car xexp) 'pre) (list (string-join (cdr xexp) ""))]
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

