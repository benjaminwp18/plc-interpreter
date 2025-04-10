#lang racket

;;;; ***************************************************
;;;; Kyle Kaufman, Benjamin Poulin, Kasey Wei
;;;; CSDS 345 Spring 2025
;;;; Group Project 3: Imperative Language Interpreter
;;;; ***************************************************

(provide binding-lookup binding-status binding-set binding-create
         binding-unbound binding-uninit binding-init empty-stt
         binding-push-layer binding-pop-layer
         binding-layer-idx binding-state-by-layer-idx)

; Return the state with an empty layer added
(define (binding-push-layer state is-func-layer?)
  (cons (empty-lyr (if is-func-layer? meta-func-lyr meta-normal-lyr)) state))

; Return the state with the first layer removed
(define (binding-pop-layer state)
  (stt-rest-lyrs state))

; Get the index of top layer in state (global layer is 1 and each layer after adds 1)
(define (binding-layer-idx state)
  (if (stt-empty? state)
      0
      (+ 1 (binding-layer-idx (stt-rest-lyrs state)))))

; Get the state with all layers above target-lyr-idx removed
; Errors if target-lyr-idx > (binding-layer-idx state)
(define (binding-state-by-layer-idx state target-lyr-idx)
  (let
      ([cur-lyr-idx (binding-layer-idx state)])
    (cond
      [(cur-lyr-idx . = . target-lyr-idx) state]
      [(cur-lyr-idx . < . target-lyr-idx) (error (~a "Tried to access state layer "
                                                     target-lyr-idx " when only " cur-lyr-idx
                                                     " exist."))]
      [else (binding-state-by-layer-idx (stt-rest-lyrs state) target-lyr-idx)])))

; Return bound value of name in state
; Error if binding does not exist
(define (binding-lookup name state [fall-thru-func-scopes? #t])
  (if (stt-empty? state)
      (error (~a name " has not been declared"))
      (let ([result (lyr-lookup-binding name (stt-first-lyr state))])
        (cond
          [(not (equal? binding-unbound result)) result]
          [(and
            (not fall-thru-func-scopes?)
            (equal? (lyr-meta-type (stt-first-lyr state)) meta-func-lyr))
           (error (~a name " has not been declared in this function"))]
          [else (binding-lookup name (stt-rest-lyrs state) fall-thru-func-scopes?)]))))

; Return the value of name in layer
; or binding-unbound if no binding for name exists in layer
(define (lyr-lookup-binding name layer)
  (cond
    [(lyr-empty? layer) binding-unbound]
    [(equal? (lyr-first-name layer) name) (unbox (lyr-first-val layer))]
    [else (lyr-lookup-binding name (lyr-of-rest-bindings layer))]))

; Return binding-unbound if name's binding does not exist in state
; Return binding-uninit if it's not initialized
; Return binding-init otherwise
(define (binding-status name state [fall-thru-func-scopes? #t])
  (with-handlers ([exn:fail? (lambda (v) binding-unbound)])
    (if (equal? (binding-lookup name state fall-thru-func-scopes?) binding-uninit)
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
        layer)]
    [else (let ([result (lyr-set-binding name value (lyr-of-rest-bindings layer))])
            (if (equal? binding-unbound result)
                binding-unbound
                (lyr-cons (lyr-first-binding layer) result)))]))

; Return state with new binding (name, value)
; Error if binding already exists
(define (binding-create name value state)
  (if (equal? (binding-status name state #f) binding-unbound)  ; Don't leave func for binding creation
      (stt-cons-lyr (lyr-create-binding name value (stt-first-lyr state)) (stt-rest-lyrs state))
      (error (~a "Binding for " name " already exists"))))

; Return this layer with an empty binding created for name
; Do not check for already existing bindings
(define (lyr-create-binding name value layer)
  (lyr-cons (list name (box value)) layer))

(define binding-unbound 'unbound)
(define binding-uninit '())
(define binding-init 'initialized)

(define stt-first-lyr car)
(define stt-rest-lyrs cdr)
(define stt-cons-lyr cons)

(define meta-func-lyr 'func-layer)
(define meta-normal-lyr 'normal-layer)

(define (empty-lyr lyr-type)
  (list (list lyr-type) '() '()))
(define empty-stt (list (empty-lyr meta-normal-lyr)))

(define (lyr-empty? layer)
  (or (null? (lyr-names layer))
      (null? (lyr-vals layer))))
(define stt-empty? null?)

(define lyr-meta car)
(define (lyr-meta-type layer) (car (lyr-meta layer)))
(define lyr-names cadr)
(define lyr-vals caddr)

(define binding-name car)
(define binding-val cadr)

(define (lyr-first-name layer)
  (car (lyr-names layer)))
(define (lyr-first-val layer)
  (car (lyr-vals layer)))
(define (lyr-first-binding layer)
  (list
   (car (lyr-names layer))
   (car (lyr-vals layer))))
(define (lyr-of-rest-bindings layer)
  (list
   (lyr-meta layer)
   (cdr (lyr-names layer))
   (cdr (lyr-vals layer))))
(define (lyr-cons pair layer)
  (list
   (lyr-meta layer)
   (cons (binding-name pair) (lyr-names layer))
   (cons (binding-val pair) (lyr-vals layer))))