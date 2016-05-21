#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit-test code.

(require rackunit)
(require rackunit/text-ui)

(require "../braids/util.rkt")
(require "game.rkt")


(define/provide oahu-mounty-start-state
  (presto-canned-state
   (vector
    (presto-player
     "Oahu" ;name
     presto-empty-zone ;hand
     presto-empty-zone ;field
     (map (lambda (ignored) (presto-name->card "Island")) (range 60)) ;library
     presto-empty-zone ;sideboard
     presto-empty-zone ;graveyard
     presto-empty-zone ;exile
     (presto-counters (list '(life . 20) '(poison . 0))) ;counters
     );/presto-player
    (presto-player
     "Mounty" ;name
     presto-empty-zone ;hand
     presto-empty-zone ;field
     (map (lambda (ignored) (presto-name->card "Mountain")) (range 60)) ;library
     presto-empty-zone ;sideboard
     presto-empty-zone ;graveyard
     presto-empty-zone ;exile 
     (presto-counters (list '(life . 20) '(poison . 0))) ;counters
     );/presto-player
    );/list

   0 ;index of person going first
   
   #f ;negative on shuffling
   );/presto-canned-state
  );/define


(define-test-suite presto-test-suite
  ;;#:before (lambda () (void))
  
  (test-case
   "103 - 103.4"  ; we test these rules from the ones published 08 Apr 2016.

   (define state oahu-mounty-start-state)  ; This is not randomized in any way.
   (define active-player (presto-state-active-player state))
   
   (check-false (void? state))  ; We're not floating in space.
   (check-eq? (presto-losing-players state) '())  ; No losers ... yet.

   ; 103.2:  Players determine who plays/draws first.
   (check-eq? (presto-state-phase state) 'starting-the-game)  ;r103
   (check-equal? (presto-player-name active-player) "Oahu")  ; Oahu gets to choose (non-random).

   (define oahu-play-first-move (presto-move active-player 'play-first (hash)))
   
   (define oahu-play-draw-moves (set oahu-play-first-move
                                     (presto-move active-player 'draw-first (hash))))
   
   (check set=? (presto-legal-moves state)
          oahu-play-draw-moves "Sets of moves are not the same.")

   (set! state (presto-perform-move state oahu-play-first-move))  ;~~~ last here
   
   (check-equal? (presto-player-name (presto-state-turn-owner state)) "Oahu")


   ; r103.4: Each player draws ... seven.
   (for ([player (presto-state-players state)])  ;r103.4
     (define actual-hand-size (length (presto-player-hand player)))
     (check-eq? actual-hand-size 7)
     );for

   (define oahu-mulligan-move (list (presto-move active-player 'mulligan (make-hash))))
   
   (check-equal? (presto-legal-moves state) oahu-mulligan-move)

   (define deets (presto-state-phase-details))

   ; no one has yet taken a mulligan.
   (check-equal? (hash-ref 'mulliganeers deets) '())

   
   
   );/test-case


  #;(test-case
   "play1"

   (define state (presto-tick oahu-mounty-start-state))
   (define active-player (presto-get-player-by-name state "Oahu"))
   (define active-player-pass (presto-move active-player 'pass (make-hash)))
   
   (check-eq? (presto-losing-players state) '())
   (check-eq? (presto-state-active-player state) active-player)
   (check-eq? (presto-state-turn-owner state) active-player)
   (check-eq? (presto-state-phase state) 'beginning)
   (check-eq? (presto-state-step state) 'upkeep)
   (check-equal? (presto-legal-moves state) (list active-player-pass))

   
   (set! state (presto-perform-move state active-player-pass))
   (set! active-player (presto-get-player-by-name state "Mounty"))
   (set! active-player-pass (list active-player 'pass))

   (check-eq? (presto-losing-players state) '())
   (check-eq? (presto-state-active-player state) active-player)
   (check-eq? (presto-state-turn-owner state) active-player)
   (check-eq? (presto-state-phase state) 'beginning)
   (check-eq? (presto-state-step state) 'upkeep)
   (check-equal? (presto-legal-moves state) active-player-pass)


   (set! state (presto-perform-move state active-player-pass))
   (set! active-player (presto-get-player-by-name state "Oahu"))
   (define active-player-draw (presto-move active-player 'draw '(from own-library)))
   (set! active-player-pass (list active-player 'pass))

   (check-eq? (presto-losing-players state) '())
   (check-eq? (presto-state-active-player state) active-player)
   (check-eq? (presto-state-turn-owner state) active-player)
   (check-eq? (presto-state-phase state) 'beginning)
   (check-eq? (presto-state-step state) 'upkeep)
   (check-equal? (presto-legal-moves state) active-player-pass)



   );/test-case
  
  ;;#:after (lambda () (void))
  )

;; Uncomment to run unit tests.
(run-tests presto-test-suite)

