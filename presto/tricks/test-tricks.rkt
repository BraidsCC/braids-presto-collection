#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit-test code.

(require rackunit)
(require rackunit/text-ui)
(require "../../braids/util.rkt")
(require "../../braids/posure.rkt")
(require "../player.rkt")
(require "question.rkt")
(require "tricks.rkt")
(require "unit-testing-rules.rkt")



(define-test-suite suite
  ;;;;;;;;;;;;;;;; -----
    (test-case
     "first-2-choices"
     (rules-state-posure-parm (make-empty-posure))

     (define player1 (tricks-player "Player One" (make-hasheq)))
     (define player2 (tricks-player "Player Two" (make-hasheq)))
     (define players (list player1 player2))

     (define salad-options (start-game unit-testing-rules players))
     (check-equal? (tricks-question-options salad-options) '(house-salad caesar-salad))

     (define entree-options (choose (tricks-question-rules-k salad-options) player1 'house-salad))
     (check-equal? (tricks-question-options entree-options) '(beef pork chicken vegetables))
     
     ;; state is in (rules-state-posure-parm).
     "end of test-case")
    
    (test-case "the end" "end of test-case"))

;; Uncomment to run unit tests.
(run-tests suite)
