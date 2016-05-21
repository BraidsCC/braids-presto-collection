#lang racket

(require "../braids/util.rkt")


(define/provide tic-tac-toe-marker-x 'x)  ; formerly x - marker

(define/provide tic-tac-toe-marker-o 'o)  ; formerly o - marker

(define/provide tic-tac-toe-marker-blank '_)  ; formerly blank - marker

(define (tic-tac-toe-marker? sym)
  (or
   (eq? sym tic-tac-toe-marker-x)
   (eq? sym tic-tac-toe-marker-o)
   (eq? sym tic-tac-toe-marker-blank)
   ) ;; end-or
  ) ;; end-define-provide

(provide (contract-out
          [tic-tac-toe-player? (any/c . -> . boolean?)]))
(define tic-tac-toe-player? tic-tac-toe-marker?)


(struct/provide tic-tac-toe-state
  (active-player
   board)
  #:transparent)

(define (tic-tac-toe-board
         ul uc ur
         cl cc cr
         ll lc lr)
  (hash 'ul ul 'uc uc 'ur ur
        'cl cl 'cc cc 'cr cr
        'll ll 'lc lc 'lr lr))


;; By default, X goes first.  Because.
(define/provide tic-tac-toe-blank-state
  (tic-tac-toe-state
   tic-tac-toe-marker-x
   (tic-tac-toe-board
         tic-tac-toe-marker-blank  tic-tac-toe-marker-blank  tic-tac-toe-marker-blank
         tic-tac-toe-marker-blank  tic-tac-toe-marker-blank  tic-tac-toe-marker-blank
         tic-tac-toe-marker-blank  tic-tac-toe-marker-blank  tic-tac-toe-marker-blank)))


(provide (contract-out
          [tic-tac-toe-canned-state (symbol?
                                     symbol? symbol? symbol?
                                     symbol? symbol? symbol?
                                     symbol? symbol? symbol?
                                     . -> . tic-tac-toe-state?)]))
(define (tic-tac-toe-canned-state active-player
                                          ul uc ur
                                          cl cc cr
                                          ll lc lr)
  (tic-tac-toe-state
   active-player
   (tic-tac-toe-board
    ul uc ur
    cl cc cr
    ll lc lr)))


(provide (contract-out
          [tic-tac-toe-active-player (tic-tac-toe-state? . -> . symbol?)]))
(define (tic-tac-toe-active-player gs)
  (tic-tac-toe-state-active-player gs))

(provide (contract-out
          [tic-tac-toe-move (tic-tac-toe-state? tic-tac-toe-player? symbol?
                                                . -> . tic-tac-toe-state?)]))
(define (tic-tac-toe-move state player position)
  (define board (tic-tac-toe-state-board state))
  (if (not (equal? (tic-tac-toe-state-active-player state) player))
      (error (~a "make-move: illegal parameter: player " player " is not the active player"))
      (if (not (equal? (hash-ref board position) tic-tac-toe-marker-blank))
          (error (~a "make-move: illegal parameter: position " position " is not blank"))
          (let
              ([next-player
                (if (equal? player tic-tac-toe-marker-o)
                    tic-tac-toe-marker-x
                    tic-tac-toe-marker-o)])
            (struct-copy tic-tac-toe-state state
                         [active-player next-player]
                         [board (hash-set board position player)])))))

(define (player-won player plank)
  (or (and (equal? (hash-ref plank 'ul) player)  ; upper row
           (equal? (hash-ref plank 'uc) player)
           (equal? (hash-ref plank 'ur) player))
      (and (equal? (hash-ref plank 'cl) player)  ; center row
           (equal? (hash-ref plank 'cc) player)
           (equal? (hash-ref plank 'cr) player))
      (and (equal? (hash-ref plank 'll) player)  ; lower row
           (equal? (hash-ref plank 'lc) player)
           (equal? (hash-ref plank 'lr) player))
      (and (equal? (hash-ref plank 'ul) player)  ; left column
           (equal? (hash-ref plank 'cl) player)
           (equal? (hash-ref plank 'll) player))
      (and (equal? (hash-ref plank 'uc) player)  ; center column
           (equal? (hash-ref plank 'cc) player)
           (equal? (hash-ref plank 'lc) player))
      (and (equal? (hash-ref plank 'ur) player)  ; right column
           (equal? (hash-ref plank 'cr) player)
           (equal? (hash-ref plank 'lr) player))
      (and (equal? (hash-ref plank 'ul) player)  ; backslash
           (equal? (hash-ref plank 'cc) player)
           (equal? (hash-ref plank 'lr) player))
      (and (equal? (hash-ref plank 'ur) player)  ; slash
           (equal? (hash-ref plank 'cc) player)
           (equal? (hash-ref plank 'll) player))))


(provide (contract-out
          [tic-tac-toe-winner (tic-tac-toe-state?
                               . -> . (or/c false? tic-tac-toe-player?))]))
(define (tic-tac-toe-winner state)
  (define board (tic-tac-toe-state-board state))
  (if (player-won tic-tac-toe-marker-o board)
      tic-tac-toe-marker-o
      (if (player-won tic-tac-toe-marker-x board)
          tic-tac-toe-marker-x
          #f)))

(provide (contract-out
          [tic-tac-toe-legal-moves-simple (tic-tac-toe-state? . -> . (listof symbol?))]))
(define (tic-tac-toe-legal-moves-simple state)
  (define (simple-helper moves-to-check)
    (if (eq? '() moves-to-check)
        ;; then
        '()
        ;; else
        (letrec 
          ([position (car moves-to-check)]
           [rest-of-result (simple-helper (cdr moves-to-check))])
          (if (equal?
               (hash-ref (tic-tac-toe-state-board state) position)
               tic-tac-toe-marker-blank)
              ;; then
              (cons position rest-of-result)
              ;; else
              rest-of-result))))
  (simple-helper '(ul uc ur cl cc cr ll lc lr)))

(provide (contract-out
          [tic-tac-toe-legal-moves (tic-tac-toe-state?
                                    . -> . (listof (list/c tic-tac-toe-player? symbol?)))]))
(define (tic-tac-toe-legal-moves state)
  (define active-player (tic-tac-toe-state-active-player state))

  (define (helper ls)
    (if (eq? '() ls)
        ;; then
        '()
        ;; else
        (cons (list active-player (car ls))
              (helper (cdr ls)))))

  (if (false? (tic-tac-toe-winner state))
      (helper (tic-tac-toe-legal-moves-simple state))
      '()
      );/if
  );/define


(provide (contract-out
          [tic-tac-toe-opponent-turn? (tic-tac-toe-state? tic-tac-toe-player? . -> . boolean?)]))
(define (tic-tac-toe-opponent-turn? state player)
  (not (eq? player (tic-tac-toe-state-active-player state))))


(define (opponent-owned? board-position player)
  (if (equal? board-position tic-tac-toe-marker-blank)
      #f
      ;; else
      (if (equal? board-position player)
          #f
          ;; else
          #t
          );/if
      );/if
  );/TL define


(provide (contract-out
          [tic-tac-toe-eval-state (tic-tac-toe-state? tic-tac-toe-player? . -> . number?)]))
(define (tic-tac-toe-eval-state state player)
  ;; --> nonnegative-integer?
  ;; state : tic-tac-toe-state?
  ;; player : tic-tac-toe-player?
  ;;
  ;; Returns an estimated strength of the game-state for the given player as a real
  ;; number from 0.0 inclusive to 1.0 inclusive.

  ;; Implementation details...
  ;; Each corner is worth 3 points.  (There are 4.)
  ;; The middle is worth 5 points;  (There is 1.)
  ;; The center of each edge is worth 2 points.  (There are 4.)
  ;; Total points available on the board = 12 + 5 + 8 = 25, so we
  ;; define a win as... 42, because it's a fun number bigger than 25.
  ;; Minimum points for a non-winning, non-losing state is 1.

  (define result 
    (let ([winner (tic-tac-toe-winner state)])
      (if (equal? winner player)
          +inf.f  ;; player won.
          ;; else
          (if (not (equal? winner #f))
              -inf.f  ;; Someone other than player won; i.e., player lost.
              ;; else  ;; No one has won yet.
              (let ([board (tic-tac-toe-state-board state)])
                (- 
                 (+
                  (if (eq? (hash-ref board 'ul) player) 3 0)
                  (if (eq? (hash-ref board 'ur) player) 3 0)
                  (if (eq? (hash-ref board 'll) player) 3 0)
                  (if (eq? (hash-ref board 'lr) player) 3 0)
                  (if (eq? (hash-ref board 'cc) player) 5 0)
                  (if (eq? (hash-ref board 'uc) player) 2 0)
                  (if (eq? (hash-ref board 'cl) player) 2 0)
                  (if (eq? (hash-ref board 'cr) player) 2 0)
                  (if (eq? (hash-ref board 'lc) player) 2 0))  ;; end-+
                 (+ 
                  (if (opponent-owned? (hash-ref board 'ul) player) 3 0)
                  (if (opponent-owned? (hash-ref board 'uc) player) 3 0)
                  (if (opponent-owned? (hash-ref board 'ur) player) 3 0)
                  (if (opponent-owned? (hash-ref board 'cl) player) 3 0)
                  (if (opponent-owned? (hash-ref board 'cc) player) 5 0)
                  (if (opponent-owned? (hash-ref board 'cr) player) 2 0)
                  (if (opponent-owned? (hash-ref board 'll) player) 2 0)
                  (if (opponent-owned? (hash-ref board 'lc) player) 2 0)
                  (if (opponent-owned? (hash-ref board 'lr) player) 2 0)
                  ) ;; /+
                 ) ;; /-
                ) ;; /let
              ) ;; /if
          ) ;; /if
      ) ;; /let
    ) ;; /define result

  ;; debug stuff.
  ;;(display (~a "(eval-state " state " " player ") --> " result))
  ;;(newline)
  
  result)  ;; end-define/provide


(provide (contract-out
          [tic-tac-toe-game-over? (tic-tac-toe-state? . -> . boolean?)]))
(define (tic-tac-toe-game-over? state)
  (eq? '() (tic-tac-toe-legal-moves state))
  );/define
