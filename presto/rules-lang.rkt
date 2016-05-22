#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax and implementations for Presto rules language.
;;
;; For a detailed example, see test-rules-lang.rkt.
;;
;; Any contract errors related to "posures" are most likely due to
;; misplacing, misnaming, or neglecting to remember a rules-variable. 


(provide (except-out (all-from-out racket) define set! let letrec require))
;; ... Other provisions are co-located with their definitions.

(require (for-syntax racket/syntax))
(require (for-syntax "../braids/util.rkt"))
(require "../braids/util.rkt")
(require "../braids/posure.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables.

;; Development option; disabling enhances performance.
(define/provide rules-lang-debug #t)
;;;;;;;;;;;;;;; ----------------

;; This is the root context for rules.  It is a global variable.  I don't like
;; this fact, but macros make it very hard to avoid global variables.
;; See (struct posure ...).
(define/provide rules-state-posure-parm (make-parameter (posure (make-hasheq) null)))
;;;;;;;;;;;;;;; -----------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros and supporting procedures.


;; This is the assignment operator for this language.
(provide :=)
(define-syntax (:= stx)
  ;;;;;;;;;;;;  --
  (syntax-case stx ()
    [(_ id val)
     (syntax/loc stx  ;; consumer-oriented error reporting
       (posure-mutate-value (rules-state-posure-parm) (quote id) val)
     )]))


;; This is the variable-fetching form, an homage to sh's dollar-notation for the same.
(provide $)
(define-syntax ($ stx)
  ;;;;;;;;;;;; ---
  (syntax-case stx ()
    [(_ id)
     (syntax/loc stx
       (posure-$ (rules-state-posure-parm) (quote id))
       )]))


(provide define/provide-rules)
(define-syntax (define/provide-rules stx)
  ;;;;;;;;;;;;  --------------------
  (syntax-case stx ()
    [(_ rule-id body ...)
     #`(begin
             (provide rule-id)
             #,(syntax/loc stx
                 (define (rule-id)
                   (let ()
                     ;(remember $section "root")
                     body ...
                     (rules-state-posure-parm)))))]))


(provide define-section)
(define-syntax (define-section stx)
  ;;;;;;;;;;;;  --------------------
  (syntax-case stx ()
    [(_ section-id body ...)
     ;; Create a new posure for this section, using the outside one as a parent.
     ;; We used a double-nested let here to aid error-reporting.
     ;; Errors occurring before and after the inner let, are likely bugs in
     ;; the rules-lang.
     
     #`(let ()
         (push-new-empty-posure!)
         ;(remember $section (symbol->string (quote section-id)))
         #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
             (let ()
               body ...))
         (pop-posure!)
         (void))]))


(define (dollar id-symbol stx)
  ;;;;;  ------
  (posure-$ (rules-state-posure-parm) id-symbol))


(define (pop-posure!)
  ;;;;;  -----------
  (rules-state-posure-parm (posure-parent (rules-state-posure-parm))))


(define (push-new-empty-posure!)
  ;;;;;  ----------------------
  (rules-state-posure-parm (posure (make-hasheq) (rules-state-posure-parm))))

(provide remember)
(define-syntax (remember stx)
  ;;;;;;;;;;;;  --------
  (syntax-case stx ()
    [(_ id val)
     (syntax/loc stx
       (posure-bind-value (rules-state-posure-parm) (quote id) val))
     ]))
