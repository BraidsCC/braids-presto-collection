#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit-test code.

(require rackunit)
(require rackunit/text-ui)

(require "../braids/braids-util.rkt")
(require "ttt.rkt")
;(require "minimax.rkt")


(define tmb tic-tac-toe-marker-blank)
(define tmo tic-tac-toe-marker-o)
(define tmx tic-tac-toe-marker-x)


(define (void/false? x) (not (or (void? x) (false? x))))


(define-test-suite tic-tac-toe-test-suite
  ;;#:before (lambda () (void))
  
  (check-pred void/false? tic-tac-toe-blank-state)
  (check-pred void/false? (tic-tac-toe-active-player tic-tac-toe-blank-state))
  
  (test-case
   "tic-tac-toe-move-x-ul, no-win"
   (let*
       ([gs tic-tac-toe-blank-state]
        [expected-board
         (hash-set (tic-tac-toe-state-board gs)
                   'ul tmx)]
        [expected-state
         (struct-copy tic-tac-toe-state gs
                      [active-player tmo]
                      [board expected-board])]
        [actual-state
         (tic-tac-toe-move gs tmx 'ul)])
     
     (check-equal? actual-state expected-state)
     (check-false (tic-tac-toe-winner actual-state))
     (check-false (tic-tac-toe-game-over? actual-state))
     (check-eq? (tic-tac-toe-opponent-turn? actual-state tmo) #f)
     (check-eq? (tic-tac-toe-opponent-turn? actual-state tmx) #t)     
     );/let*
   );/test-case
  
  
  (test-case
   "test-win"
   (let*
       ([gs (tic-tac-toe-move
             (tic-tac-toe-move
              (tic-tac-toe-move
               (tic-tac-toe-move
                (tic-tac-toe-move
                 tic-tac-toe-blank-state ; Start with empty board,
                 tmx 'cc) ; and then x moves to center center.  In order:
                tmo 'uc) ; o to upper center,
               tmx 'ur) ; x to upper right,
              tmo 'cl) ; o to center left,
             tmx 'll) ; x to lower left for the win...
            ])

     (check-equal? (tic-tac-toe-winner gs) tmx)
     (check-true (tic-tac-toe-game-over? gs))

     (check-true (< (tic-tac-toe-eval-state gs tmo)
                    (tic-tac-toe-eval-state gs tmx)))

     (check-equal? (tic-tac-toe-eval-state gs tmx) +inf.f)
     (check-equal? (tic-tac-toe-eval-state gs tmo) -inf.f)
     
     ) ;; end-let*
   ) ;; end test-case
  
  (test-case
   "player-takes-two-turns"
   (check-exn
    exn:fail?
    (lambda ()
      (tic-tac-toe-move
       (tic-tac-toe-move
        tic-tac-toe-blank-state ; empty board
        tmx 'cc) ; x to center center
       tmx 'uc)))) ; x to upper center (throws exception)
  
  (test-case
   "players-play-on-same-square"
   (check-exn
    exn:fail?
    (lambda ()
      (tic-tac-toe-move
       (tic-tac-toe-move
        tic-tac-toe-blank-state ; empty board
        tmx 'cc) ; x to center center
       tmo 'cc)))) ; o to center center

  (test-case
   "test-tic-tac-toe-legal-moves"
   (let*
       ([gs (tic-tac-toe-move
             (tic-tac-toe-move
              (tic-tac-toe-move
               tic-tac-toe-blank-state ; Start with empty board,
               tmx 'cc) ; and then x moves to center center.  In order:
              tmo 'uc) ; o to upper center,
             tmx 'ur)]) ; x to upper right.

     (let ([expected-state
            (tic-tac-toe-canned-state  tmo
                                       tmb tmo tmx
                                       tmb tmx tmb
                                       tmb tmb tmb)])
       ;; gs and expected-state must be equal?.
       (check-equal? gs expected-state))

     ;; Make sure o has an inferior board-position.
     (check-true (< (tic-tac-toe-eval-state gs tmo)
                    (tic-tac-toe-eval-state gs tmx)))

     ;; It is o's turn, and only o's turn.
     (check-equal?
      (tic-tac-toe-state-active-player gs)
      tmo)

     ;; Game isn't over yet.
     (check-false (tic-tac-toe-game-over? gs))

     ;; Check simple list of available moves
     (check-equal?
      (tic-tac-toe-legal-moves-simple gs)
      '(ul cl cr ll lc lr))

     ;; Check fuller list of available moves; these include all the parameters
     ;; needed to actually make a move.
     (check-equal?
      (tic-tac-toe-legal-moves gs)
      (list
       (list tmo 'ul)
       (list tmo 'cl)
       (list tmo 'cr)
       (list tmo 'll)
       (list tmo 'lc)
       (list tmo 'lr)))))

  
  ;;#:after (lambda () (void))
  )

;; Uncomment to run unit tests.
(run-tests tic-tac-toe-test-suite)

