#lang racket

(require "../../braids/util.rkt")
(require "../player.rkt")


(struct/provide/contract-out tricks-question
                             ;--------------
                             ([rules-k continuation?]
                              [player tricks-player?]
                              [options list?])
                             #:transparent)
