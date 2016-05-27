#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lang module for tricks, the rules language for presto.
;;
;; For a detailed example of how to use the language, see
;; "unit-testing-rules.rkt."
;;
;; Any contract errors related to "posures" are most likely due to
;; misplacing, misnaming, or neglecting to remember a rules-variable. 

(require (for-syntax racket/syntax))
(require (for-syntax "../../braids/util.rkt"))

(require "../../braids/util.rkt")
(require "../../braids/posure.rkt")
(require "../player.rkt")
(require "decision.rkt")
(require "options.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source-level configuration options.



;; Development option; disabling may enhance performance.
(define/provide tricks-debug-flag #t)
;;;;;;;;;;;;;;; -----------------



(provide (except-out (all-from-out racket) define set! let letrec require)
         (all-from-out "../player.rkt"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct-use procedures and macros.
;;
;; This block contains macros and procedures that one would normally use in
;; Tricks rules-definitions.



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



;; Build an options struct to get back a decision struct.
(define/provide/contract-out (ask outside-k player question)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ---
  
  (-> continuation? tricks-player? any/c  any/c)
  (let
      ([decision (let/cc inside-k
                   (outside-k (tricks-options inside-k player question)))])
    
    (cond [(tricks-debug-flag . and . (not (tricks-decision? decision)))
           (define message (~a "ask (via continuation): contract violation\n"
                               " expected: tricks-decision?\n"
                               " given:\n"
                               "   " decision))
           (raise (make-exn:fail:contract message (current-continuation-marks)))])
    
    ;; "Leap frog" the external continuation:
    (:= external-k (tricks-decision-controller-k decision))
    (tricks-decision-choice decision)) ; this is the choice made externally.
  )



(define/provide/contract-out (choose inside-k player selected-val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ------

  (-> continuation? tricks-player? any/c  tricks-options?)

  ;; Counterpart to ask:  build a decision struct to get the next options struct.
  (let
      ([options (let/cc outside-k
                   (inside-k (tricks-decision outside-k player selected-val)))])
       (cond [(tricks-debug-flag . and . (not (tricks-options? options)))
              (define message (~a "choose (via continuation): contract violation\n"
                                  " expected: tricks-options?\n"
                                  " given:\n"
                                  "   " options))
              (raise (make-exn:fail:contract message (current-continuation-marks)))])
    options))
  

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
       (-> (listof tricks-player?) continuation?  void?)
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

  

(define/provide/contract-out (first-game-of-match?)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------------------
  (-> boolean?)
  (false? ($ previous-game-player-who-decided-who-played-first)
   ))



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


  
(define/provide/contract-out (previous-game-was-a-draw?)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------------------
  (-> boolean?)
  (false? ($ prior-game-losing-player)))



(define/provide/contract-out (random-player)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------
  (-> tricks-player?)
  (car (shuffle ($ players))))



(define-syntax-case/provide (remember stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  [(_ id val)
   (syntax/loc stx
     (posure-bind-value (rules-state-posure-parm) (quote id) val))
   ])



(define/provide/contract-out (shuffle! player zone-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  (-> tricks-player? symbol?  void?)
  (set-player-zone! player zone-sym (shuffle (player-zone player zone-sym))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internals:  These forms and procedures are not intended for direct use
;; in rules-definitions, but are necessary for them to work and/or for other
;; code to use them.
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables.



(define-for-syntax controller-k-var-symbol
  ;;;;;;;;;;;;;;;; -----------------------
  (string->unreadable-symbol "tricks-$controller-k-var-symbol"))



(define-for-syntax internal-loop-var-symbol (string->unreadable-symbol "tricks-$internal-loop-var"))
;;;;;;;;;;;;;;;;;; ------------------------



(define-for-syntax players-var-symbol (string->unreadable-symbol "tricks-$players-var"))
;;;;;;;;;;;;;;;;;; ------------------



;; This is the root context for rules.  It is a global variable.  I don't like
;; this fact, but macros make it very hard to avoid global variables.
;; See (struct posure ...).
(define/provide rules-state-posure-parm (make-parameter (posure (make-hasheq) null)))
;;;;;;;;;;;;;;; -----------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros and procedures.



(define (dollar id-symbol)
  ;;;;;  ------
  (posure-$ (rules-state-posure-parm) id-symbol))


  
(define (pop-posure!)
  ;;;;;  -----------
  (rules-state-posure-parm (posure-parent (rules-state-posure-parm))))


  
(define (push-new-empty-posure!)
  ;;;;;  ----------------------
  (rules-state-posure-parm (posure (make-hasheq) (rules-state-posure-parm))))



  
(define-syntax-case/provide (remember-symbol stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---------------
  [(_ symbol val)
   (syntax/loc stx
     (posure-bind-value (rules-state-posure-parm) symbol val))
   ])


  
(define (remember-section/players/external-k player-list external-k)
  ;;;;;  -----------------------------------
  ;(-> (listof tricks-player?) continuation?   any)
  (remember section "root")
  (remember players player-list)
  (remember-symbol 'external-k external-k))



(define/provide/contract-out (start-game rules players)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ----------
  (-> procedure? (listof tricks-player?)  tricks-options?)
  (let/cc k (rules players k)))
