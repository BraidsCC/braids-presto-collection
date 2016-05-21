#lang racket

(require "../braids/util.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs

(struct/provide/contract-out presto-move ((player presto-player?)
                                          (action symbol?)
                                          (parameters hash?)
                                          )
                             #:transparent)

(struct/provide/contract-out presto-player ((name string?)
                                            (hand presto-zone?)
                                            (field presto-zone?)
                                            (library presto-zone?)
                                            (sideboard presto-zone?) ;100.4
                                            (graveyard presto-zone?)
                                            (exile presto-zone?)
                                            (counters presto-counters?)
                                            )
                             ;#:transparent
                             )

(struct/provide/contract-out presto-state ((players (listof presto-player?))
                                           (turn-owner presto-player?)
                                           (active-player presto-player?) ;102.1
                                           (phase symbol?)
                                           (step symbol?)
                                           (stack list?)
                                           (awaiting-stack-order list?)
                                           (players-who-have-passed (listof presto-player?))
                                           (shuffle boolean?)
                                           (phase-details hash?)
                                           (moves-cache (or/c void? (set/c presto-move?)) #:mutable)
                                           )
                             #:transparent
                             )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Values (constants)

(define/provide presto-empty-zone '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures

(define/provide/contract-out (presto-canned-state players active-index shuffle)
  (-> (vectorof presto-player?) ;players
      exact-nonnegative-integer?  ;active-index - which player in players (vector-ref) is going first
      boolean? ;shuffle
      presto-state?)

  (define active-player (vector-ref players active-index))
  
  (presto-state (vector->list players) ;players
                active-player ;turn-owner
                active-player ;active-player
                'starting-the-game ;phase
                'play-or-draw ;step
                '() ;stack
                '() ;awaiting-stack-order
                '() ;players-who-have-passed
                the-empty-hash ;phase-details
                shuffle  ;shuffle
                (void) ;moves-cache (not yet built)
                )
  );/define


(define/provide/contract-out (presto-card? val)
  (->           any/c  ;val
                boolean?)
  (string? val))

(define/provide/contract-out (presto-counter? val)
  (->              any/c  ;val
                   boolean?)
  (and (pair? val)
       (symbol? (car val))))


(define/provide/contract-out (presto-counters ls)
  (->              (listof presto-counter?)  ;ls
                   presto-counters?
                   )
  (apply hasheq (flatten ls)))


(define/provide presto-counters? hash-eq?)


(define/provide/contract-out (presto-get-player-by-name state player-name)
  (-> presto-state? ;state
      string? ; player-name
      presto-player?)

  (define matching-players (filter (lambda (p) (equal? player-name (presto-player-name p)))
                                   (presto-state-players state)))
  
  (cond
    [(empty? (cdr matching-players))
     (car matching-players)]  ; only one result, yay!
    [(empty? matching-players)
     (error "No player found by that name")]
    [else
     (error "Multiple players found having that name")]
    )
  );/def/pro/con


(define/provide/contract-out (presto-legal-moves state)
  (-> presto-state? ;state
   (set/c presto-move?))

  (define (compute-moves-no-cache state)

    (define active-player (presto-state-active-player state))
    
    (case (presto-state-phase state)
      [(starting-the-game)  ;phase
       (case (presto-state-step state)
         [(play-or-draw) ;step
          (set (presto-move active-player 'play-first the-empty-hash)
               (presto-move active-player 'draw-first the-empty-hash))
          ]
         [else (set)]
         ); case on step
       ] ;starting-the-game phase
      [(beginning) ;phase
       (case (presto-state-step state)
         [(upkeep)  ; similar to ending phase, end step
          ;; Query hands and field to see if players can do anything.  Assuming not...
          (set (presto-move active-player 'pass the-empty-hash))
          ];upkeep step
         [else (set)]
         );case on step
       
       ];beginning phase
      [else (set)]
      );case on phase
    );def
  
  (define (fetch-cache) (presto-state-moves-cache state))
  
  (cond
    [(void? (fetch-cache))
     (display (~a "moves-cache miss\n"))
     (set-presto-state-moves-cache! state (compute-moves-no-cache state))
     ]
    [else
     (display (~a "moves-cache hit\n"))
     ]
    );cond

  (fetch-cache)
    
  );def


(define/provide/contract-out (presto-losing-players state)
  (-> presto-state?  ;state
      (listof presto-player?))
  
  (define players (presto-state-players state))
  (filter presto-player-lost? players)
  );/define


(define/provide/contract-out (presto-name->card cheese)
  (-> string? ; cheese
      presto-card?)
  
  cheese)


(define/provide/contract-out (presto-perform-move state move)
  (-> presto-state?  ;state
   presto-move?  ;move
   presto-state?)

  ;; quick security/sanity check
  (if (set-member? (presto-legal-moves state) move)
      'awesome
      (error "attempted to perform illegal move"))

  (case (presto-move-action move)
    [else
     (error (~a "don't know how to perform move: " move))
     ]
    );case
  );def


(define/provide/contract-out (presto-player-lost? player)
  (-> presto-player?  ;player
      boolean?)

  (define counters (presto-player-counters player))
  (or ((hash-ref counters 'poison) . >= . 10)
      ((hash-ref counters 'life) . <= . 0))
  );/define


(define/provide/contract-out (presto-tick state)
  (-> presto-state?
      presto-state?)

  (define (nyi-error-string phase step)
    (~a "not implemented: phase " phase ", step " step))

  (define phase (presto-state-phase state))
  (define step (presto-state-step state))
  (define moves (presto-legal-moves state))

  (cond
    [(not (empty? moves))
     (error
      (~a "cannot step forward while waiting for player(s) to select a move; moves = " moves))
      ]
    [else
     (case phase
       [(beginning)
        (case step
          [(untap)
           (define intermediate-state (presto-untap-all state))
           (struct-copy presto-state intermediate-state [step 'upkeep])]
          [else
           (error (nyi-error-string phase step))
           ];case-else
          );case on step
        ];beginning phase
       [else
        (error (nyi-error-string phase step))
        ];case-else
       );case on phase
     ];cond-else
    );cond
  );def


(define/provide/contract-out (presto-untap-all state)
  (-> presto-state?
      presto-state?)

  ;~~~ untap all cards in fields.
  state
  );/defprocon


(define/provide/contract-out (presto-winner state)
  (-> presto-state?
      (or/c presto-player? false?))

  #f  ;~~~ implement
  );def

(define/provide/contract-out (presto-zone? val)
  (-> any/c ;val
      boolean?)

  (list? val))
