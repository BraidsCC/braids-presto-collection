#lang racket

(require "../braids/util.rkt")

(struct/provide/contract-out presto-zone
                             ;------------
                             ([contents list?]))  ; not transparent!
