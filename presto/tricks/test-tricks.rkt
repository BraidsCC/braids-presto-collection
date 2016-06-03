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

(define (make-first-choice first-choice) 
  (define player1 (presto-player "Player One" (make-hasheq)))
  (define player2 (presto-player "Player Two" (make-hasheq)))
  (define players (list player1 player2))
  
  (define start-options (start-game unit-testing-rules players))
  
  (define st (tricks-question-options start-options))
  
  (check set-member? st first-choice
         (~a "make-first-choice: not a member of initial choices"))
  
  (check-equal? (tricks-question-player start-options) player1)
  
  (list players (choose-for start-options player1 first-choice)))




(define-test-suite suite
  ;;;;;;;;;;;;;;;; -----
  (test-case
   "sections"
   
   (define first-choice-result (make-first-choice 'sections))
   ;(define players (car first-choice-result))
   ;(define player1 (first players))
   ;(define player2 (second players))
   
   "end of test-case")
  
  
  (test-case
   "ask-bomb-bad-choice"
   
   (define first-choice-result (make-first-choice 'ask-bomb-bad-choice))
   (define players (car first-choice-result))
   (define player1 (first players))
   
   (check-exn exn:fail:contract?
              (lambda () (choose-for first-choice-result player1 'boom)))
   
   "end of test-case")
  
  
  (test-case
   "ask-bomb-empty-set"
   
   (check-exn exn:fail:contract?
              (lambda () (make-first-choice 'ask-bomb-empty-set)))
   
   "end of test-case")
  
  
  (test-case
   "ask-inside-procedure"
   
   (define first-choice-result (make-first-choice 'ask-inside-procedure))
   (define players (car first-choice-result))
   (define player1 (first players))

   (define proc-question (second first-choice-result))
   (check-equal? (tricks-question-options proc-question) (set 'in-proc))

   (define next-question (choose-for proc-question player1 'in-proc))
   (check-equal? (tricks-question-options next-question) (set 'in-rules))
   
   "end of test-case")


  (test-case
   "dinner"
   
   (define first-choice-result (make-first-choice 'dinner))
   (define players (car first-choice-result))
   (define player1 (first players))
   (define player2 (second players))
   
   (define salad-set (set 'house-salad 'caesar-salad))
   
   (define salad-options1 (second first-choice-result))
   (check-equal? (tricks-question-options salad-options1) salad-set)
   (check-equal? (tricks-question-player salad-options1) player1)
   
   (define salad-options2 (choose-for salad-options1 player1 'caesar-salad))
   (check-equal? (tricks-question-options salad-options2) salad-set)
   (check-equal? (tricks-question-player salad-options2) player2)
   
   (define entree-set (set 'beef 'pork 'chicken 'vegetables))
   
   (define entree-options (choose-for salad-options2 player2 'house-salad))
   (check-equal? (tricks-question-options entree-options) entree-set)
   (check-equal? (tricks-question-player entree-options) player1)
   
   "end of test-case")
  
  (test-case "the end" "end of test-case"))

;; Uncomment to run unit tests.
(run-tests suite)
