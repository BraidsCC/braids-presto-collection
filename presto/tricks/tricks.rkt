#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lang module for tricks, the rules language for presto.
;;
;; For a detailed example of how to use the language, see unit-test-rules.rkt.
;;
;; Any contract errors related to "posures" are most likely due to
;; misplacing, misnaming, or neglecting to remember a rules-variable. 


(require "../player.rkt")
(require "procedures.rkt")
(require "macros.rkt")

(provide (except-out (all-from-out racket) define set! let letrec require)
         (all-from-out "../player.rkt")
         (all-from-out "procedures.rkt")
         (all-from-out "macros.rkt"))
