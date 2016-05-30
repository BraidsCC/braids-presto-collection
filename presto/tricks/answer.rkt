#lang racket

(require "../../braids/util.rkt")
(require "../player.rkt")         


(struct/provide/contract-out tricks-answer
                             ;------------
                             ([controller-k continuation?]
                              [player presto-player?]
                              [choice any/c])
                             #:transparent)
