;;;;;;;;;;;;;;;;;;;; THIS MODULE IS DEPRECATED. ;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;; Other provisions are co-located with their definitions.
(provide (except-out (all-from-out racket) define set! let letrec require))

(require (for-syntax racket/syntax))
(require "../braids/util.rkt")


;; Development option.
(define rules-lang-debug #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The rules language has support for inspectable closures.  Because they are
;; implemented as mutable hash tables, we call them "posures," a portmanteau
;; of poser (faker) and closure.
;;
;; In the context of a posure, the terms "parent" and "enclosing posure" are
;; synonymous.



;; This internal Racket variable keeps track of the current section's closure.
;; Each new section re-binds this using let.  When the section ends, the
;; usual garbage-collection takes place.
(define-for-syntax state-var-symbol 'rules$state-posure)


(struct posure (bindings parent) #:transparent)


(define-syntax (raise-undefined-rules-variable-error stx)
  (syntax-case stx ()
    [(_ proc-name-string variable-name-symbol)
     #'(raise (make-exn:fail:contract:variable
               (~a proc-name-string ": undefined rules-variable\n"
                   "  variable-name: " variable-name-symbol)
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
     (with-syntax
         ([state-posure-id (format-id stx "~a" state-var-symbol)])
       #'(begin
           (provide rule-id)
           (define (rule-id)
             (let
                 (;[section-id '()]
                  [state-posure-id (posure (make-hash) null)])
               ;(remember $section "root")
               body ...
               state-posure-id
               ))))]))


(provide define-section)
(define-syntax (define-section stx)
  (syntax-case stx ()
    [(_ section-id body ...)
     (with-syntax
         ([state-posure-id (format-id stx "~a" state-var-symbol)])
       #'(let
             ;; Create a new posure for this section, using the outside one as a parent.
             (
              [state-posure-id (posure (make-hash) state-posure-id)]
              )
           
           ;; I'd rather put this in the let that use set!, but Racket has heartburn with that...
           ;; ... or does it?
           ;(set! state-posure-id (posure (make-hash) state-posure-id))
           ;(remember $section (symbol->string (quote section-id)))
           body ...
           ;(set! state-posure-id (posure (make-hash) state-posure-id))
           (void)))]))


(provide remember)
(define-syntax (remember stx)
  (syntax-case stx ()
    [(_ id val)
     (with-syntax
         ([state-posure-id (format-id stx "~a" state-var-symbol)])
       #'(posure-bind-value state-posure-id (quote id) val)
           )]))


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
         ([state-posure-id (format-id stx "~a" state-var-symbol)])
       #'(posure-fetch-value state-posure-id (quote id)))]))


;; This is the assignment operator for this language.
(provide :=)
(define-syntax (:= stx)
  (syntax-case stx ()
    [(_ id val)
     (with-syntax
         ([state-posure-id (format-id stx "~a" state-var-symbol)])
       #'(let ()
           (posure-mutate-value state-posure-id (quote id) val)
           (void)))]))

;(define/provide/struct-out rules-state ([]))