#lang racket

(require "../braids/util.rkt")

(define-syntax-case/provide (ask stx)
  [(_ outside-k question)
   #'(let ([list-from-outside (let/cc inside-k (outside-k (list inside-k question)))])
       (set! outside-k (car list-from-outside))  ;; "Leap frog" the external continuation.
       (cadr list-from-outside))])

(define (choose choice-context-k choice)
  (if (not (void? choice))
      (let/cc k (choice-context-k (list k choice)))
      (call/cc choice-context-k)))


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
(define (order-beverage decider-k)
  (define question '(coffee tea))
  
  (define choice1 (ask decider-k question))
  
  (display (~a "order-beverage: What kind of " choice1 " would you like?\n"))
  
  (set! question
        (cond
          [(eq? choice1 'coffee) '(black decaf)]
          [else '(iced hot)]))

  (define choice2 (ask decider-k question))
  
  (define beverage-name (~a choice2 " " choice1))
  
  ;; We can't return normally from this procedure, so we store the result in
  ;; one last call to the outside continuation.
  (decider-k (list #f beverage-name))
  'unreachable)  ; unless outside-k isn't a continuation... which would be really weird.



(define first-result (choose order-beverage (void)))
(define first-inside-k (car first-result))
(define first-choices (cadr first-result))

(define kafe-result (choose first-inside-k (car first-choices)))
(define kafe-inside-k (car kafe-result))
(define kafe-choices (cadr kafe-result))

(define decaf-result (choose kafe-inside-k (cadr kafe-choices)))
decaf-result

;; We could use time-travel to choose tea instead.
;(define tea-r (choose (car first-result) 'tea))
