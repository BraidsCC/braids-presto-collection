#lang racket

;; Demonstrates a sort of "segmented computation" in a soft-of
;; "generator" style.
;;
;; It periodically calls outside-k with a list of two elements in one of two forms:
;; either a continuation (leading back inside this procedure) and a list of choices, or
;; #f and the final result.
;;
;; To advance the computation, call the continuation last provided by this procedure,
;; passing a list in the form (k choice), where k is the continuation produced
;; by the previous call, and choice is one of the values in the choices-list.
;;
;; This procedure never actually returns.
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
;; To reduce confusion and errors in usage, I could wrap calls to this in a
;; Racket generator.  Doing this while still supporting time-travel is an
;; exercise left to the reader.
;;
(define (order-beverage outside-k)
  ;; This demonstrates an interesting capability of continuations.
  ;; This procedure performs calculations, and, when it needs more information, it
  ;; asks by invoking the external continuation.
  ;;
  ;; From a lexical view, these calculations are linear -- that is, they are not
  ;; nested.  This is relevant, because one way to implement this is by nesting
  ;; lambdas.
  ;;
  (display "order-beverage: May I take your order?\n")
  (display "order-beverage: Please pass your choice into the procedure.\n")
  
  ;; "be sure to include uh, a self-addressed, stamped, ... continuation...?"
  
  (define question #f)
  (define resume-values #f)
  
  ;; (the author is aware of the gratuitous use of set! throughout this procedure.)
  (set! question '(coffee tea))
  
  (set! resume-values (let/cc inside-k (outside-k (list inside-k question))))
  (set! outside-k (car resume-values))
  (define choice1 (cadr resume-values))
  
  (display (~a "order-beverage: What kind of " choice1 " would you like?\n"))
  
  (set! question
        (cond
          [(eq? choice1 'coffee) '(black decaf)]
          [else '(iced hot)]))
  
  (set! resume-values (let/cc yield-k (outside-k (list yield-k question))))
  (set! outside-k (car resume-values))
  (define choice2 (cadr resume-values))
  
  (display (~a "order-beverage: I'll make sure it's " choice2 ".\n"))
  (define beverage-name (~a choice2 " " choice1))
  
  (display (~a "order-beverage: One " beverage-name " -- coming up!\n"))
  
  ;; We can't return normally from this procedure, so we store the result in
  ;; one last call to the outside continuation.
  (outside-k (list #f beverage-name))
  "unreachable")



(define r1 (call/cc order-beverage))

(display (~a "coder: Coffee, please.  Here's my continuation.\n"))
(define kafe-r (let/cc k ((car r1) (list k 'coffee))))

(display (~a "coder: Black.  Here's my latest continuation.\n"))
(define black-r (let/cc k ((car kafe-r) (list k 'black))))

(display (~a "\n"
             "coder: Oops, I accidentally ordered "
             black-r
             "; I'll use my time machine to get something else.\n"
             "(REWINDING SOUND)\n"
             "\n"))

; continuations allow for time-travel to the computational past.
(display (~a "coder: Tea.\n"))
(define tea-r (let/cc k ((car r1) (list k 'tea))))

(display (~a "coder: Earl grey.  Hot.\n"))
(define hot-r (let/cc k ((car tea-r) (list k 'hot))))

(display (~a "\n"
             "coder: Ahh, I finally got my "
             (cadr hot-r)
             " (with a side of "
             (car hot-r)
             "on the left).\n"))
