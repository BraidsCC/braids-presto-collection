#lang racket

(require "util.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The posure type and related procedures.
;;
;; This implements inspectable (and externally mutable!) closures as hash
;; tables.  Because of this implementation choice,
;; we call them "posures," a portmanteau
;; of "poser" (faker) and "closure".
;;
;; In the context of a posure, the terms "parent" and "enclosing posure" are
;; synonymous.

(struct/provide/contract-out posure
;;;;;;;;;;;;;;;;;;;;;;;;;;;; ------
                             ([bindings (and/c (not/c immutable?) hash-eq?)]
                              [parent (or/c posure? null?)]) #:transparent) 


;; Returns the value for the given symbol in either this or an enclosing posure.
;; Throws exn:fail:contract:variable if the symbol is not found or if po is null.
;;
;; This is also known as the fetch operation;
;; the $ is an homage to the UNIX Bourne shell.
(define/provide/contract-out (posure-$ po symbol)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  (-> (or/c posure? null) symbol?   any/c)
  (cond
    [(null? po) (raise-undefined-posure-variable-error "posure-$" symbol)]
    [else (hash-ref (posure-bindings po)
                    symbol
                    (lambda () (posure-$ (posure-parent po) symbol)))]))


;; Binds a symbol to a value (a la let/define) in the innermost posure.
;; Returns (void).
(define/provide/contract-out (posure-bind-value po symbol value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -----------------
  (-> posure? symbol? any/c  void?)
  (hash-set! (posure-bindings po) symbol value)
  (void))


;; Returns the posure (or one of its parent (enclosing) posures) where the symbol is bound; this is
;; guaranteed to never be null.
;; 
;; Raises exn:fail:contract:variable if the symbol is not bound or if po is null.
(define (posure-find po symbol)
  ;;;;;  -----------
  (cond
    [(null? po)
     (raise-undefined-posure-variable-error "posure-fetch-value" symbol)]
    [(hash-has-key? (posure-bindings po) symbol)
     po]
    [else
     (posure-find (posure-parent po) symbol)]))


;; Changes a symbol's value at the posure where it was bound,
;; which may be an enclosing posure.
(define/provide/contract-out (posure-mutate-value po symbol value)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------------
  (-> posure? symbol? any/c   void?)
  (let ([binder (posure-find po symbol)])
    (posure-bind-value binder symbol value)))


;; Raises an exn:fail:contract:variable exception with a Racket-style message;
;; this is a macro, because, in theory, this helps Racket get the
;; source location (srcloc) right.
;;
;; @todo Is this really needed?
(define-syntax (raise-undefined-posure-variable-error stx)
  ;;;;;;;;;;;;  -------------------------------------
  (syntax-case stx ()
    [(_ proc-name-string variable-name-symbol)
     #'(raise (make-exn:fail:contract:variable
               (undefined-posure-variable-error-message proc-name-string variable-name-symbol)
               (current-continuation-marks)
               variable-name-symbol))]))


;; Returns a Racket-style error message for not being able to find a variable in a posure.
(define/provide/contract-out (undefined-posure-variable-error-message proc-name var-sym-or-str)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ---------------------------------------
  (-> (or/c string? symbol?) (or/c string? symbol?)   string?)
  (~a proc-name ": undefined rules-variable\n"
      "  variable-name: " var-sym-or-str))
