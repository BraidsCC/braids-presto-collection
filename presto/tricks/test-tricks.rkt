#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit-test code.

(require rackunit)
(require rackunit/text-ui)
(require "../../braids/util.rkt")
(require "../player.rkt")
(require "question.rkt")
(require "tricks.rkt")
(require "unit-testing-rules.rkt")



(define-test-suite suite
  ;;;;;;;;;;;;;;;; -----
    (test-case
     "first-2-choices"

     (define player1 (presto-player "Player One" (make-hasheq)))
     (define player2 (presto-player "Player Two" (make-hasheq)))
     (define players (list player1 player2))

     (define salad-options1 (start-game unit-testing-rules players))
     (check-equal? (tricks-question-options salad-options1) '(house-salad caesar-salad))
     (check-equal? (tricks-question-player salad-options1) player1)

     (define salad-options2 (choose-for salad-options1 player1 'caesar-salad))
     (check-equal? (tricks-question-options salad-options2) '(house-salad caesar-salad))
     (check-equal? (tricks-question-player salad-options2) player2)
     
     (define entree-options (choose-for salad-options2 player2 'house-salad))
     (check-equal? (tricks-question-options entree-options) '(beef pork chicken vegetables))
     (check-equal? (tricks-question-player entree-options) player1)

     "end of test-case")
    
    (test-case "the end" "end of test-case"))

;; Uncomment to run unit tests.
(run-tests suite)
