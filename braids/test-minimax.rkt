#lang racket

(require profile)


(require rackunit)
(require rackunit/text-ui)

(require "util.rkt")
(require "../tic-rack-toe/game.rkt")
(require "minimax.rkt")




(define __ tic-tac-toe-marker-blank)
(define oh tic-tac-toe-marker-o)
(define ex tic-tac-toe-marker-x)

(define (best-move-ttt state player depth)
  (best-move state player depth
             tic-tac-toe-opponent-turn?
             tic-tac-toe-legal-moves
             tic-tac-toe-move
             tic-tac-toe-eval-state
             )
  )

(define (display-board board-hash)
  (define (at-position sym)
    (hash-ref board-hash sym))
  
  (define offset "     ")
  (display (~a offset " " (at-position 'ul) " | " (at-position 'uc) " | " (at-position 'ur)))
  (newline)
  (display (~a offset "---+---+---"))
  (newline)
  (display (~a offset " " (at-position 'cl) " | " (at-position 'cc) " | " (at-position 'cr)))
  (newline)
  (display (~a offset "---+---+---"))
  (newline)
  (display (~a offset " " (at-position 'll) " | " (at-position 'lc) " | " (at-position 'lr)))
  (newline)
  );/define

(define (one-step state)
  (define minimax-depth 12)
  (display "Thinking ...")
  (newline)
  (define move (best-move-ttt state (tic-tac-toe-state-active-player state) minimax-depth))
  (display (~a "Selected move is " move "."))
  (newline)
  
  (define result-state (apply tic-tac-toe-move state move))
  
  (newline)
  (define board-hash (tic-tac-toe-state-board result-state))
  (display-board board-hash)
  (newline)
  
  result-state
  );/define

(define (until-winner state)
  (let ([state-prime (one-step state)])
    (if (not (tic-tac-toe-game-over? state-prime))
        (until-winner state-prime)
        state-prime))
  );/define until-winner



(define-test-suite minimax-test-suite
  (test-case
   "trivial-o-win"

   (let*
       (
        [state (tic-tac-toe-canned-state  oh
                                          ex oh ex
                                          ex oh __
                                          __ __ __)]
        [expected (list oh 'lc)]
        [best-move-one (best-move-ttt state oh 1)]
        )
     (check-equal? best-move-one expected)
     (check-equal? (best-move-ttt state oh 2) expected)
     (check-equal? (best-move-ttt state oh 3) expected)
     (check-equal? (best-move-ttt state oh 4) expected)
     (check-equal? (best-move-ttt state oh 5) expected)
     );/let
   );/test-case

  (test-case
   "watch-them-tie"


   (define expected-tie-state
     (profile
      (until-winner tic-tac-toe-blank-state)
      #:repeat 1);/profile
     );/define
   

   (check-true (tic-tac-toe-game-over? expected-tie-state))
   (check-false (tic-tac-toe-winner expected-tie-state))
   );/test-case

  (test-case
   "regression-bbx-bxb-oxo"

   (define (regression-bbx-bxb-oxo)
     (let*
         ([state (tic-tac-toe-canned-state  oh
                                            __ __ ex
                                            __ ex __
                                            oh ex oh)]
          [expected (list oh 'uc)]
          [actual (best-move-ttt state oh 5)]
          )
       (equal? actual expected)
       );/let
     );/define

   
   (check-true (regression-bbx-bxb-oxo))
   );/test-case regression bbx bxb oxo

  (test-case
   "regression-bbo-bxb-xbo"
   (define state
     (tic-tac-toe-canned-state ex
                               __ __ oh
                               __ ex __
                               ex __ oh))
   (define player ex)
   (define depth 2) ; Fails unless this is at least 2
   (define expected (list player 'cr))
   (define actual (best-move-ttt state player depth))
   (check-equal? actual expected)
   );/test-case

  );/define-test-suite

(run-tests minimax-test-suite)


