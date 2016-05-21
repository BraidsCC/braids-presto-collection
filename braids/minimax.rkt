#lang racket

(require "util.rkt")


; Memoized game states to reduce depth of computations
(define/provide minimax-rate-child-moves-cache (make-hash))

(define/provide minimax-inverse-cache-hits 0)


(provide (contract-out
          [best-move (any/c any/c exact-nonnegative-integer?  ; state player depth
                            (any/c any/c . -> . boolean?)  ; opponent-turn?
                            (any/c . -> . list?)  ; legal-moves
                            procedure?  ; make-move
                            (any/c any/c . -> . number?)  ; eval-state
                            . -> . any)]
          ));/provide /contract-out

(define (best-move state player depth opponent-turn? legal-moves make-move eval-state)
  ;; --> x s.t. (or (void? x) (set-member? (apply legal-move-proc state) x))
  ;; state : any
  ;; player : any
  ;; depth : number?
  ;; opponent-turn? : procedure?
  ;; legal-moves : procedure?
  ;; make-move : procedure?
  ;; eval-state : procedure?
  ;;
  ;; (opponent-turn? any-state any-player) --> boolean?
  ;; (legal-moves any-state) --> list? , specifically, (move1 [moveN ...] )
  ;; (make-move any-state1 move) --> any-state2
  ;; (eval-state any-state any-player) --> number?
  ;;
  ;; We use a generalized minimax implementation to determine the best move for the player.
  ;;
  ;; In short, to minimax *any* turn-based game, we need...
  ;; * state, which is a snapshot of a game's entire state in a single value;
  ;; * player, which is mainly used for opponent-turn? and eval-state;
  ;; * depth, which limits the computational intensity;
  ;; * opponent-turn?, a means to determine whether it is that player's turn for a given state;
  ;; * legal-moves, a procedure that produces a list of legal moves for a given state  -- it is VERY
  ;;   important that this returns '() if the game is over;
  ;; * make-move, a procedure that takes a move and returns a new state therefrom;
  ;; * and eval-state, a heuristic function to evaluate a specific state's usefulness to a
  ;;   particular player.  Usually, one implements this s.t. +inf.f is a win, and -inf.f is a loss
  ;;   for that player.
  ;;
  ;; If no player can make any legal moves, we return (void).
  ;;
  ;; Regarding computational intensity and depth, this procedure is is O(expt n depth), where n is
  ;; approximately (length (legal-moves state)).

 ;(define (terminal-state? state1) (eq? '() (legal-moves state1)))
  
  (define (rate-child-moves-helper parent-state moves prior-best-move prior-best-strength depth)
    (if (void? prior-best-move)
        (error (~a "rate-child-moves: prior-best-move must never be void"))
        #f)
    (define result
      (cond [(eq? moves '())
             (cons prior-best-move prior-best-strength)
             ]
            [else
             ;( display (~a "rate-child move " (car moves) "\n"))
             (define child-move (car moves))
             (define child-state (apply make-move parent-state child-move))
             (define child-strength (cdr (minimax child-state (- depth 1))))
             (define better-move
               ;; ~~~todo
               ;; I'd like to add more intelligence here, such as choosing moves that win
               ;; earlier over ones that win later.  That requires returning depth telemetry
               ;; (or such) from the minimax procedure.
               ;;
               ;; For now, we just pick whichever is the highest.  For moves that have
               ;; equal strengths, we bias first toward
               ;; winning/losing moves (for non-opponents' and opponents' turns,
               ;; respectively).  We bias second toward moves that are later in the list of
               ;; moves.
               ;;
               (if (or (and (opponent-turn? parent-state player)
                            (<= child-strength prior-best-strength))  ; minimize
                       (and (not (opponent-turn? parent-state player))
                            (>= child-strength prior-best-strength))) ; maximize
                   child-move
                   prior-best-move))
             (define better-strength
               (cond [(eq? better-move child-move)
                     ;( display (~a "no, " child-move " is better ; strength = " child-strength "\n"))
                      child-strength]
                     [else prior-best-strength])
               );/define better-strength
                        
             (rate-child-moves-helper parent-state (cdr moves) better-move better-strength depth)
             ];/else
            );/cond
      );/define result
    result
    );/define rate-child-moves-helper

  ;(define (rate-child-moves-1 parent-state moves prior-best-move prior-best-strength depth)
  ;  (define use-cache #t);;;;;
  ;
  ;  (define relevant-args-list (list parent-state player depth))  ;;~~~ remove depth
  ;
  ;  ;; Returns (depth move . strength)
  ;  (define (helper-thunk)
  ;  ;  (cons
  ;  ;   depth
  ;     (rate-child-moves-helper parent-state moves prior-best-move prior-best-strength depth))
  ;  ;  );/define invoke-helper-thunk
  ;
  ; 
  ;
  ;  ;; Experimental code to determine if inverse opponents' moves are ever searched.
  ;  ;(define sample-opponent-ttt (if (eq? player 'x) 'o 'x))
  ;  ;
  ;  ;(define inverse-relevant-args-list (list parent-state sample-opponent-ttt depth))
  ;  ;
  ;  ;
  ;  ;(cond
  ;  ;  [(not (void? (hash-ref minimax-rate-child-moves-cache inverse-relevant-args-list (void))))
  ;  ;   (add1! minimax-inverse-cache-hits)])
  ;
  ;  (cond
  ;    [use-cache
  ;     (hash-ref!/tail minimax-rate-child-moves-cache relevant-args-list helper-thunk)]
  ;    [else
  ;     (helper-thunk)
  ;     ];/else
  ;    );/cond
  ;
  ;  );/define rate-child-moves

  
  ; We only memoize calls to this that are not directly recursive, because the only thing that
  ; changes is (the list of) moves and the best move so far.
  ; For any given game state, minimax is always going to call
  ; this first.
  ;
  (define (rate-child-moves parent-state moves prior-best-move prior-best-strength depth)
    (define use-cache #t)

    (define relevant-args-list (list parent-state player depth))  ;;~~~ remove depth

    ;; ~~~ to eventually return (depth move . strength)
    (define (helper-thunk)
    ;  (cons
    ;   depth
       (rate-child-moves-helper parent-state moves prior-best-move prior-best-strength depth))
    ;  );/define invoke-helper-thunk

    

    ;; Experimental code to determine if inverse opponents' moves are ever searched.
    ;(define sample-opponent-ttt (if (eq? player 'x) 'o 'x))
    ;
    ;(define inverse-relevant-args-list (list parent-state sample-opponent-ttt depth))
    ;
    ;
    ;(cond
    ;  [(not (void? (hash-ref minimax-rate-child-moves-cache inverse-relevant-args-list (void))))
    ;   (add1! minimax-inverse-cache-hits)])

    (cond
      [use-cache
       (hash-ref!/tail minimax-rate-child-moves-cache relevant-args-list helper-thunk)]
      [else
       (helper-thunk)
       ];/else
      );/cond
    
    );/define rate-child-moves

  (define (minimax theoretical-state depth)
    (define moves (legal-moves theoretical-state))
    (define result
      (cond
        [(or (equal? depth 0) (eq? '() moves))
          (define strength (eval-state theoretical-state player))
          (cons (void) strength)
          ]
        [else
         (if (not (opponent-turn? theoretical-state player))
             (rate-child-moves theoretical-state moves (car moves) -inf.f depth)
             ;;else    (* minimizing player *)
             (rate-child-moves theoretical-state moves (car moves) +inf.f depth)
             );/if
          
          ];/else
        );/cond
      );/define result
    result
    )


  (define minimax-pair (minimax state depth))
  (display (~a ":(best-move ...) --> " minimax-pair "\n"))
  (car minimax-pair)
  );/define


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression-test code

;(require "../tic-rack-toe/ttt.rkt")
;    
;(define __ tic-tac-toe-marker-blank)
;(define oh tic-tac-toe-marker-o)
;(define ex tic-tac-toe-marker-x)
;
;(define (best-move-ttt state player depth)
;  (best-move state player depth
;             tic-tac-toe-opponent-turn?
;             tic-tac-toe-legal-moves
;             tic-tac-toe-move
;             tic-tac-toe-eval-state))
;
;
;(if (regression-bbx-bxb-oxo)
;    (display "regression test passed")
;    (display "regression test FAILED"))
;(newline)
