#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit-test code.

;(require racket/control)
;(require racket/generator)
(require rackunit)
(require rackunit/text-ui)

(require "../../braids/util.rkt")
(require "../../braids/posure.rkt")
(require "../player.rkt")

;(require "procedures.rkt")
(require "macros.rkt")

(require "unit-testing-rules.rkt")






(define-test-suite suite
    (test-case
     "first-choice"
     (rules-state-posure-parm (make-empty-posure))

     (define player1 (player "Player One" (make-hasheq)))
     (define player2 (player "Player Two" (make-hasheq)))

     (define players (list player1 player2))

     (define options (start-game unit-testing-rules players))
     
     (check-equal? (tricks-options-options options) '(house-salad caesar-salad))
     
     ;; state is in (rules-state-posure-parm).
     "end of test-case")
    
    (test-case "the end" "end of test-case"))

;; Uncomment to run unit tests.
(run-tests suite)
