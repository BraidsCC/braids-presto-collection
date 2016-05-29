#lang racket

(require "../braids/util.rkt")
(require "zone.rkt")


(struct/provide/contract-out presto-player
                             ;------------
                             ([name string?]
                              [zone-dict dict?] ; maps zone-symbol? => presto-zone?
                              )
                             #:transparent)

(define/provide/contract-out (make-player p-name lib-card-names side-card-names)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -----------
  (-> string? (listof string?) (listof string?)  presto-player?)
  (presto-player p-name (make-hasheq (list (list 'library lib-card-names)
                                    (list 'sideboard side-card-names)))))

;; not to be confused with player-zones, which is plural  :-/
(define/provide/contract-out (player-zone player zone-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -----------
  (-> presto-player? symbol?  presto-zone?)
  (hash-ref (presto-player-zone-dict player) zone-sym))


(define/provide/contract-out (set-player-zone! player zone-sym zone)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ----------------
  (-> presto-player? symbol? presto-zone?  void?)
  (hash-ref! (presto-player-zone-dict player) zone-sym zone)
  (void))
