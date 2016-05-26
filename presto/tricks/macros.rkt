#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internals for tricks, the presto rules language.
;;
;; Some convenience procedures are in "procedures.rkt".
;;
;; For a detailed example, of a rules definition, see "unit-test-rules.rkt".
;;
;; Any contract errors related to "posures" are most likely due to
;; misplacing, misnaming, or neglecting to [remember ...] a rules-variable. 


(require (for-syntax racket/syntax))
(require (for-syntax "../../braids/util.rkt"))
(require "../../braids/util.rkt")
(require "../../braids/posure.rkt")
(require "../player.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables.



(define-for-syntax controller-k-var-symbol
  ;;;;;;;;;;;;;;;; -----------------------
  (string->unreadable-symbol "tricks-$controller-k-var-symbol"))



(define-for-syntax internal-loop-var-symbol (string->unreadable-symbol "tricks-$internal-loop-var"))
;;;;;;;;;;;;;;;;;; ------------------------



(define-for-syntax players-var-symbol (string->unreadable-symbol "tricks-$players-var"))
;;;;;;;;;;;;;;;;;; ------------------



;; Development option; disabling may enhance performance.
(define/provide tricks-debug-flag #t)
;;;;;;;;;;;;;;; -----------------



;; This is the root context for rules.  It is a global variable.  I don't like
;; this fact, but macros make it very hard to avoid global variables.
;; See (struct posure ...).
(define/provide rules-state-posure-parm (make-parameter (posure (make-hasheq) null)))
;;;;;;;;;;;;;;; -----------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros and procedures supporting them.



;; This is the assignment operator for this language.
(define-syntax-case/provide (:= stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---
  [(_ id val)
   (syntax/loc stx  ;; consumer-oriented error reporting
     (posure-mutate-value (rules-state-posure-parm) (quote id) val)
     )])



;; This is the variable-fetching form, an homage to sh's dollar-notation for the same.
(define-syntax-case/provide ($ stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;; ---
  [(_ id)
   (syntax/loc stx
     (dollar (quote id))
     )])



(define-syntax-case/provide (ask stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---
  [(_ outside-k player question)
   ;; Build an options struct to get back a decision struct.
   #'(let
         ([decision (let/cc inside-k
                      (outside-k (tricks-options inside-k player question)))])
       ;; "Leap frog" the external continuation:
       (cond [(tricks-debug-flag . and . (not (tricks-decision? decision)))
             (define message (~a "ask: contract violation\n"
                                 " expected: tricks-decision?\n"
                                 " given:\n"
                                 "   decision "))
             (raise (make-exn:fail:contract message (current-continuation-marks)))])
             
       (:= external-k (tricks-decision-controller-k decision))
       (tricks-decision-choice decision)) ; this is the choice made externally.
   ])



(define/provide/contract-out (choose decision)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ------
  (-> tricks-decision?  tricks-options?)

  ;; Counterpart to ask:  use the decision struct to get back an options struct.
  (define controller-k (tricks-decision-controller-k decision))
  (define choice (tricks-decision-choice decision))
  (let/cc k (controller-k decision)))

  

(define-syntax-case/provide (define/provide-rules stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  --------------------
  [(_ (rules-id players external-k) body ...)
;   (with-syntax
;       ([players (datum->syntax stx players-var-symbol)]
;        [decider-k (datum->syntax stx controller-k-var-symbol)]
;        ;; more?
;        )
     ; =>
     ;; Do not invoke this directly; see start-game:
   #`(define/provide/contract-out (rules-id players external-k)
       (-> (listof player?) continuation?  void?)
       (remember-section/players/external-k players external-k)
       #,(syntax/loc stx
           (let ()
             body ...))
       (void))
   ])



(define-syntax-case/provide (define-section stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  --------------
  [(_ section-id body0 bodyN ...)
   ;; Create a new posure for this section, using the outside one as a parent.
   ;; We used a double-nested let here to aid error-reporting.
   ;; Errors occurring before and after the inner let, are likely bugs in
   ;; Tricks, which is why we used syntax-case for the body.
   ;; =>
   #`(let ()
       (push-new-empty-posure!)
       (remember $section (symbol->string (quote section-id)))
       #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
           (let ()
             body0 bodyN ...))
       (pop-posure!)
       (void))])

  

(define (dollar id-symbol)
  ;;;;;  ------
  (posure-$ (rules-state-posure-parm) id-symbol))


  
;; Loops over all players in any order. While not yet implemented as an
;; asynchronous operation, it's a cue should that ever become desirable.
;;
;; Syntax:
;; (for-each-player-in-parallel variable-name
;;   body+ ...)
;;
;; Example:
;; (for-each-player-in-parallel that-player
;;   (shuffle! ($ that-player) 'library))
;;
(define-syntax-case/provide (for-each-player-in-parallel stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---------------------------
  [(_ rules-var-id body0 bodyN ...)
   (with-syntax
       ([internal-var-id (datum->syntax stx 'internal-loop-var-symbol)])
     #`(begin
         (push-new-empty-posure!)
         (remember rules-var-id #f)
         (for ([internal-var-id ($ players)])
           (:= rules-var-id internal-var-id)
           #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
               (let ()
                 body0 bodyN ...))
           )
         (pop-posure!)))])


  
(define (pop-posure!)
  ;;;;;  -----------
  (rules-state-posure-parm (posure-parent (rules-state-posure-parm))))


  
(define (push-new-empty-posure!)
  ;;;;;  ----------------------
  (rules-state-posure-parm (posure (make-hasheq) (rules-state-posure-parm))))



  
(define-syntax-case/provide (remember stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  [(_ id val)
   (syntax/loc stx
     (posure-bind-value (rules-state-posure-parm) (quote id) val))
   ])



(define-syntax-case/provide (remember-symbol stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---------------
  [(_ symbol val)
   (syntax/loc stx
     (posure-bind-value (rules-state-posure-parm) symbol val))
   ])


  
(define (remember-section/players/external-k player-list external-k)
  ;;;;;  -----------------------------------
  ;(-> (listof player?) continuation?   any)
  (remember section "root")
  (remember players player-list)
  (remember-symbol 'external-k external-k))



(define/provide/contract-out (start-game rules players)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ----------
  (-> procedure? (listof player?)  tricks-options?)
  (let/cc k (rules players k)))



(struct/provide/contract-out tricks-options
                             ;-------------
                             ([rules-k continuation?]
                              [player player?]
                              [options list?])
                             #:transparent)


  
(struct/provide/contract-out tricks-decision
                             ;--------------
                             ([controller-k continuation?]
                              [player player?]
                              [choice any/c])
                             #:transparent)


