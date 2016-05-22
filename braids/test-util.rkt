#lang racket

(require "util.rkt")

(module submod racket
  (require "util.rkt")

  ;(struct/provide test-struct-type (n b) #:transparent )
  
  ;(provide (struct-out test-struct-type))
  
  (struct/provide/contract-out test-struct-type ((n number?) (b boolean?)))

  (struct/provide/contract-out test-struct-type2 ((n number? #:mutable)))

  (struct/provide/contract-out test-struct-type3 ((n number? #:mutable)
                                                  (b boolean?)
                                                  (ls list? #:mutable)
                                                  (h hash?)))

  (struct/provide/contract-out test-struct-type4 ((n number?)
                                                  (b boolean? #:mutable)
                                                  (ls list?)
                                                  (h hash? #:mutable))
                               #:transparent)

  (define/provide/contract-out
    (add2 val) (number? . -> . number?)
    (add1 (add1 val))
    );dpco

  (define-syntax-case/provide (add1-macro stx)
    [(_ expr)
     #`(add1 expr)])
  
  );/module




(require 'submod)

;(define t1 (test-struct-type 42 20))  ; fails

(define/provide s4 (test-struct-type4 5 #f '(1 2) (make-hash)))

(add1-macro 3)

"Unless you see errors above, the tests most likely passed.  Most of them are syntactic, anyway."
