#lang racket

(require "../braids/util.rkt")


(struct/provide/contract-out tricks-player
                             ;----;
                             ([name string?]
                              [zones hash-eq?] ; maps zone-symbol? => zone?
                              ))

(define/provide/contract-out (make-player p-name lib-card-names side-card-names)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -----------
  (-> string? (listof string?) (listof string?)  tricks-player?)
  (tricks-player p-name (make-hasheq (list (list 'library lib-card-names)
                                    (list 'sideboard side-card-names)))))

;; not to be confused with player-zones, which is plural  :-/
(define/provide/contract-out (player-zone player zone-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -----------
  (-> tricks-player? symbol?  zone?)
  (hash-ref (tricks-player-zones player) zone-sym))


(define/provide/contract-out (set-player-zone! player zone-sym zone)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ----------------
  (-> tricks-player? symbol? zone?  void?)
  (hash-ref! (tricks-player-zones player) zone-sym zone)
  (void))


(define/provide/contract-out (zone? val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -----
  (-> any/c  boolean?)  
  (list? val))
