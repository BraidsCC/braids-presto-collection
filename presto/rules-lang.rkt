#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap lang-module for for Presto rules language.
;;
;; For a detailed example of how to use the language, see test-rules-lang.rkt.
;;
;; Any contract errors related to "posures" are most likely due to
;; misplacing, misnaming, or neglecting to remember a rules-variable. 


(require (for-syntax racket/syntax))
(require (for-syntax "../braids/util.rkt"))
(require "../braids/util.rkt")
(require "../braids/posure.rkt")
(require "player.rkt")
(require "rules-procedures.rkt")
(require "rules-macros.rkt")

(provide (except-out (all-from-out racket) define set! let letrec require)
         (all-from-out "player.rkt")
         (all-from-out "rules-procedures.rkt")
         (all-from-out "rules-macros.rkt"))
