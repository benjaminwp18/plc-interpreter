#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 2: Flow Control Interpreter
;;;; ***************************************************

(provide binding-lookup binding-status binding-set binding-create
         binding-unbound binding-uninit binding-init empty-stt stt-empty?
         binding-push-layer binding-pop-layer)

; Return the state with an empty layer added
(define (binding-push-layer state)
  (cons empty-lyr state))

; Return the state with the first layer removed
(define (binding-pop-layer state)
  (stt-rest-lyrs state))

; Return bound value of name in state
; Error if binding does not exist
(define (binding-lookup name state)
  (if (stt-empty? state)
      (error (~a name " has not been declared"))
      (let ([result (lyr-lookup-binding name (stt-first-lyr state))])
        (if (equal? binding-unbound result)
            (binding-lookup name (stt-rest-lyrs state))
            (unbox result)))))

; Return the value of name in layer
; or binding-unbound if no binding for name exists in layer
(define (lyr-lookup-binding name layer)
  (cond
    [(lyr-empty? layer) binding-unbound]
    [(equal? (lyr-first-name layer) name) (lyr-first-val layer)]
    [else (lyr-lookup-binding name (lyr-cdr layer))]))

; Return binding-unbound if name's binding does not exist in state
; Return binding-uninit if it's not initialized
; Return binding-init otherwise
(define (binding-status name state)
  (with-handlers ([exn:fail? (lambda (v) binding-unbound)])
    (if (equal? (binding-lookup name state) binding-uninit)
        binding-uninit
        binding-init)))

; Return state with value set for name's binding
; Error if binding does not exist
(define (binding-set name value state)
  (if (stt-empty? state)
      (error (~a name " has not been declared"))
      (let ([result-lyr (lyr-set-binding name value (stt-first-lyr state))])
        (if (equal? binding-unbound result-lyr)
            (stt-cons-lyr (stt-first-lyr state) (binding-set name value (stt-rest-lyrs state)))
            (stt-cons-lyr result-lyr (stt-rest-lyrs state))))))

; Return this layer with the binding for name set to value
; or binding-unbound if no binding for name exists in layer
(define (lyr-set-binding name value layer)
  (cond
    [(lyr-empty? layer) binding-unbound]
    [(equal? (lyr-first-name layer) name)
      (begin
        (set-box! (lyr-first-val layer) value) 
        (lyr-cons (list name value) (lyr-cdr layer)))]
    [else (let ([result (lyr-set-binding name value (lyr-cdr layer))])
            (if (equal? binding-unbound result)
                binding-unbound
                (lyr-cons (lyr-car layer) result)))]))

; Return state with new binding (name, value)
; Error if binding already exists
(define (binding-create name value state)
  (if (equal? (binding-status name state) binding-unbound)
      (stt-cons-lyr (lyr-create-binding name value (stt-first-lyr state)) (stt-rest-lyrs state))
      (error (~a "Binding for " name " already exists"))))

; Return this layer with an empty binding created for name
; Do not check for already existing bindings
(define (lyr-create-binding name value layer)
  (lyr-cons (list name value) layer))

(define binding-unbound "unbound")
(define binding-uninit '())
(define binding-init "initialized")

(define stt-first-lyr car)
(define stt-rest-lyrs cdr)
(define stt-cons-lyr cons)

(define empty-lyr '(() ()))
(define empty-stt (list empty-lyr))

(define (lyr-empty? layer)
  (or (null? (lyr-names layer))
      (null? (lyr-vals layer))))
(define stt-empty? null?)

(define lyr-names car)
(define lyr-vals cadr)
(define binding-name car)
(define binding-val cadr)
(define (lyr-first-name layer)
  (car (lyr-names layer)))
(define (lyr-first-val layer)
  (car (lyr-vals layer)))
(define (lyr-car layer)
  (list
   (car (lyr-names layer))
   (car (lyr-vals layer))))
(define (lyr-cdr layer)
  (list
   (cdr (lyr-names layer))
   (cdr (lyr-vals layer))))

(define (lyr-cons pair layer)
  (list
   (cons (binding-name pair) (lyr-names layer))
   (cons (box (binding-val pair)) (lyr-vals layer))))