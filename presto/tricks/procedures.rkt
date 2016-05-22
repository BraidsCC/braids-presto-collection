#lang racket

(require "../../braids/util.rkt")
(require "../../braids/posure.rkt")
(require "macros.rkt")
(require "../player.rkt")


(define/provide/contract-out (first-game-of-match?)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------------------
  (-> boolean?)
  (false? ($ previous-game-player-who-decided-who-played-first)
   ))


(define/provide/contract-out (previous-game-was-a-draw?)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------------------
  (-> boolean?)
  (false? ($ prior-game-losing-player)))

(define/provide/contract-out (random-player)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------
  (-> player?)
  (car (shuffle ($ players))))


(define/provide/contract-out (shuffle! player zone-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  (-> player? symbol?  void?)
  (set-player-zone! player zone-sym (shuffle (player-zone player zone-sym))))
