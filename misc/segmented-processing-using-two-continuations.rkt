#lang racket

;; Demonstrates a sort of "segmented computation" in a soft-of
;; "generator" style.
;;
;; Typical usage example:
;;
;; (call/cc here-k (order-beverage here-k)) --> (there-k0 (list choice1 choice2))
;;
;; Capture there-k into a variable and call it with one of the choices:
;; 
;; (let/cc here-k (there-k0 choice))
;;
;; which, again, yields a result (there-k2 value).  Eventually, one call to there-k
;; produces (#f valueN), indicating that valueN is the final result (but not a
;; choice).
;;
(define (order-beverage outside-k)
  (define question '(coffee tea))
  
  (define list-from-outside (let/cc inside-k (outside-k (list inside-k question))))
  (set! outside-k (car list-from-outside))  ;; "Leap frog" the external continuation.
  (define choice1 (cadr list-from-outside))
  
  (display (~a "order-beverage: What kind of " choice1 " would you like?\n"))
  
  (set! question
        (cond
          [(eq? choice1 'coffee) '(black decaf)]
          [else '(iced hot)]))
  
  (set! list-from-outside (let/cc yield-k (outside-k (list yield-k question))))
  (set! outside-k (car list-from-outside))
  (define choice2 (cadr list-from-outside))
  
  (define beverage-name (~a choice2 " " choice1))
  
  ;; We can't return normally from this procedure, so we store the result in
  ;; one last call to the outside continuation.
  (outside-k (list #f beverage-name))
  "unreachable")



(define first-result (call/cc order-beverage))
(define first-inside-k (car first-result))
(define first-choices (cadr first-result))

(define kafe-result (let/cc k (first-inside-k (list k (car first-choices)))))
(define kafe-inside-k (car kafe-result))
(define kafe-choices (cadr kafe-result))

(define decaf-result (let/cc k (kafe-inside-k (list k (cadr kafe-choices)))))
decaf-result
