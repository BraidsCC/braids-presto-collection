#lang racket

(provide (except-out (all-from-out racket) define set! let letrec require))
;; ... Other provisions are co-located with their definitions.

(require (for-syntax racket/syntax))
(require (for-syntax "../braids/util.rkt"))
(require "../braids/util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables.

;; Development option; disabling enhances performance.
(define/provide rules-lang-debug #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The rules language has support for inspectable closures.  Because they are
;; implemented as mutable hash tables, we call them "posures," a portmanteau
;; of poser (faker) and closure.
;;
;; In the context of a posure, the terms "parent" and "enclosing posure" are
;; synonymous.



(struct posure (bindings parent) #:transparent)

;(provide
; (struct-out
;  [struct posure ([bindings (and/c (not/c immutable?) hasheq?)]
;                  [parent (or/c posure? null?)])]))


;; This is the root context for rules.  It is a global variable.  I don't like
;; this fact, but macros make it very hard to avoid global variables.
;; See (struct posure ...).
(define/provide state-posure (posure (make-hasheq) null))  ;; rename-to rules-lang-posures


(define (undefined-rules-variable-error-message proc-name-string variable-name-symbol)
  (~a proc-name-string ": undefined rules-variable\n"
      "  variable-name: " variable-name-symbol))



(define-syntax (raise-undefined-rules-variable-error stx)
  (syntax-case stx ()
    [(_ proc-name-string variable-name-symbol)
     #'(raise (make-exn:fail:contract:variable
               (undefined-rules-variable-error-message proc-name-string variable-name-symbol)
               (current-continuation-marks)
               variable-name-symbol))]))


;; Returns the value for the given symbol in either this or an enclosing posure.
;; Throws exn:fail:contract:variable if the symbol is not found or if po is null
(define (posure-fetch-value po symbol)
  (cond
    [(null? po)
     (raise-undefined-rules-variable-error "posure-fetch-value" symbol)
     ]
    )
  (hash-ref (posure-bindings po)
            symbol
            (lambda () (posure-fetch-value (posure-parent po) symbol))))


;; Returns the posure (or one of its parent (enclosing) posures) where the symbol is bound; this is
;; guaranteed to never be null.
;; 
;; If the symbol is not bound or if po is null, throws an exn:fail:contract:variable value.
(define (posure-find po symbol)
  (cond
    [(null? po)
     (raise-undefined-rules-variable-error "posure-fetch-value" symbol)]
    [(hash-has-key? (posure-bindings po) symbol)
     po]
    [else
     (posure-find (posure-parent po) symbol)]))


;; Binds a symbol to a value (a la let/define) in the innermost posure.
;; Returns (void).
;;
;; If rules-lang-debug is #t, and the po is null, this raises exn:fail:contract.
(define (posure-bind-value po symbol value)
  (cond [(and rules-lang-debug (null? po)) (raise-argument-error 'po "(not/c null?)" po)])
  
  (hash-set! (posure-bindings po) symbol value)
  (void))


;; Changes a symbol's value at the posure where it was bound,
;; which may be an enclosing posure.
(define (posure-mutate-value po symbol value)
  (let ([binder (posure-find po symbol)])
    (posure-bind-value binder symbol value)))


(provide define/provide-rules)
(define-syntax (define/provide-rules stx)
  (syntax-case stx ()
    [(_ rule-id body ...)
     #'(begin
         (provide rule-id)
         (define (rule-id)
           (let ()
             ;(remember $section "root")
             body ...
             state-posure)))]))


(define (push-new-empty-posure!)
  (set! state-posure (posure (make-hasheq) state-posure)))

(define (pop-posure!)
  (set! state-posure (posure-parent state-posure)))


(provide define-section)
(define-syntax (define-section stx)
  (syntax-case stx ()
    [(_ section-id body ...)
     #'(let ()
         ;; Create a new posure for this section, using the outside one as a parent.
         
         (push-new-empty-posure!)
         ;(remember $section (symbol->string (quote section-id)))
         body ...
         (pop-posure!)
         (void))]))


(provide remember)
(define-syntax (remember stx)
  (syntax-case stx ()
    [(_ id val)
     #'(posure-bind-value state-posure (quote id) val)
     ]))


#;(provide remember2)
#;(define-syntax (remember2 stx)
    (syntax-case stx ()
      [(_ id val)
       (with-syntax
           ([state-posure-id (format-id stx "~a" state-var-symbol)])
         #'(begin
             (posure-bind-value state-posure-id (quote id) val)
             (define (id) (posure-fetch-value state-posure-id (quote id)))))]))

;; This is the variable-fetching form, an homage to sh's dollar-notation for the same.
(provide $)
(define-syntax ($ stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax
         (
          ;[srcloc (syntax->srcloc->syntax stx)]
          [stx stx])  ;; ~~~ do I need to run datum->syntax on this?  ... doubtful.
       #'(dollar 'id stx)
       )]))


(define (dollar id-symbol stx)
  (with-handlers
      ([exn:fail:contract:variable?
       (lambda (exn)
         (define message (undefined-rules-variable-error-message "$" id-symbol))
         (raise (make-exn:fail:syntax (exn-message exn)
                                      (current-continuation-marks)
                                      (list stx))))])
    (posure-fetch-value state-posure (quote id))) stx)


;; This is the assignment operator for this language.
(provide :=)
(define-syntax (:= stx)
  (syntax-case stx ()
    [(_ id val)
     #'(let ()
         (posure-mutate-value state-posure (quote id) val)
         (void))]))

;(define/provide/struct-out rules-state ([]))