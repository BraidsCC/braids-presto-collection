#lang racket

(module pie racket
  (struct paperclip ())
  (provide (struct-out paperclip))

  (struct cup (contents))
  (provide (contract-out
            [struct cup ((contents (listof paperclip)))]))
  );mod

(require 'pie)
(define best-mom (cup '(oops)))
