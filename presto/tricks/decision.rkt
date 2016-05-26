#lang racket

(require "../../braids/util.rkt")
(require "../player.rkt")         


(struct/provide/contract-out tricks-decision
                             ;--------------
                             ([controller-k continuation?]
                              [player tricks-player?]
                              [choice any/c])
                             #:transparent)
