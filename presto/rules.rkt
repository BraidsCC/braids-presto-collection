#lang s-exp "rules-lang.rkt"


;; There are three kinds of values in the game's state we care about.
;;
;; Position-evaluation relevant values (pervs) contribute to the game's state
;; as it pertains to the (a) position evaluation heuristic.  Those values
;; must be available outside whatever closures the rules might export.
;;
;; Game-state values upon which impromptu rules (cards) may rely.
;;
;; All other values, which usually only have bearing on which questions we
;; ask of the next person... and tend to not stick around all that long.
;;
;; ... bah, it doesn't matter.  Keep all of them.  It's simpler.





; 103.1
(define/provide-rules presto-rules
  (remember first-game-of-match? 'nyi)
  (remember random-player 'nyi)
  (remember prior-game-was-a-draw? 'nyi)
  (remember for-each-player-in-parallel 'nyi)
  (remember set-life-total 'nyi)
  (remember that-player 'nyi)
  (remember for-each-player 'nyi)
  (remember draw-cards 'nyi)
  (remember consult-cards 'nyi)
  (remember allow-mulligan 'nyi)
  (remember for-each-player-starting-with 'nyi)
  (remember remember-prior-mulligan 'nyi)
  (remember reset-mulligan-decision 'nyi)
  (remember mulligan-decision-is 'nyi)
  (remember previous-mulligan? 'nyi)
  (remember hand-size 'nyi)
  (remember decision 'nyi)
  (remember mulligan-decision? 'nyi)
  (remember move-all 'nyi)
  (remember draw 'nyi)
  
  
  
  (define-section match  ; A match has one or more games, all with the same players.
    ; 103.2 also implies keeping track of who won the preivous match, and who
    ; chose who went first in that match.
    
    (remember previous-game-player-who-decided-who-played-first #f)
    (remember players '())
    (remember prior-game-losing-player #f)
    
    (define-section game
      (define-section beginning-of-game
        (remember starting-player #f)
        
        
        (for-each-player-in-parallel (shuffle that-player 'library))
        
        ; 103.1a is programmatically unnecessary.
        ; 103.1b is for commander variant.
        ; 103.1c is for conspiracy variant.      
        
        
        ; 103.2:
        
        ;(determine-seating-order); 103.2 team-play
        
        (:= starting-player ; to...
              (cond  
                [(first-game-of-match?)
                 (random-player)]
                [(prior-game-was-a-draw?)
                 
                 ;;~~~ if an effect can intercept an action, then the effect
                 ;; can change the state.  Most actions are interruptible.
                 ;; (This also applies to ask, because someone may clone the entire state
                 ;; between asking and answering.  I'm not going to mention any names ("minimax")).
                 ;; I'm not sure yet where this state originates.
                 ;; if I did this in, say, Python, ask would eventually call something
                 ;; with a do-this-next-procedure.  That procedure would accept
                 ;; ask's return value and a (possibly) new state.  This is like call/cc,
                 ;; but the continuation uses a struct, ignoring all free variables.
                 ;;
                 ;; Providing seamless state-updates in the middle of a loop is important,
                 ;; too.
                 ;; For each loop, we might have to explicitly add remember-forms for the loop's
                 ;; iteration-context.  That is easier if we name each loop uniquely.  The loop
                 ;; could also
                 ;; create a stage automatically or something...
                 ;;
                 (ask previous-game-player-who-decided-who-played-first
                      (cartesian-product '(play-first) (players)))]
                [else
                 (ask prior-game-losing-player (cartesian-product '(play-first) (players)))]))
        
        ; 103.2a is for shared team turns.
        ; 103.2b is for archenemy.
        ; 103.2c conspiracy card "Power Play" overrides these rules.
        
        ; 103.3:
        (for-each-player-in-parallel (set-life-total that-player 20))
        
        ; 103.3a 2hg
        ; 103.3b vanguard
        ; 103.3c commander
        ; 103.3d archenemy
        
        ;103.4:
        ; Draw cards.
        (for-each-player ;103.4
         (draw-cards
          (or (consult-cards 'hand-size) 7)))
        
        (for-each-player-in-parallel
         (allow-mulligan that-player))  ;~~~ last here

        (define-stage mulligans
          (remember done-with-mulligans #f)

          (while (not (done-with-mullgans))
            ; First, find out who is taking a mulligan.
            (for-each-player-starting-with
             starting-player
             
             (remember-prior-mulligan that-player)
             (reset-mulligan-decision that-player)
             
             (mulligan-decision-is            
              (if (and (previous-mulligan? that-player) ((hand-size that-player) . > . 0))
                  (ask that-player ('mulligan-yes 'mulligan-no))
                  ;; else cannot mulligan
                  (decision that-player ('mulligan-no))
                  );if
              );mulligan-dec
             );for-each-player
            
            (for-each-player-in-parallel
             (cond
               [(mulligan-decision? that-player)
                (define hand-size (hand-size that-player))
                
                (move-all that-player 'hand 'library)
                (shuffle that-player 'library)
                (draw (sub1 hand-size))
                ]
               );cond
             );for
            
            ;~~~more
            );while
          );mulligans
        
        );beginning-of-game scope
      
      );game scope
    );match scope
  );rules
