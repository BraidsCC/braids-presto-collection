#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax and implementations for Presto rules language.
;;
;; For a detailed example, see test-rules-lang.rkt.
;;
;; Any contract errors related to "posures" are most likely due to
;; misplacing, misnaming, or neglecting to remember a rules-variable. 


(require (for-syntax racket/syntax))
(require (for-syntax "../braids/util.rkt"))
(require "../braids/util.rkt")
(require "../braids/posure.rkt")
(require "player.rkt")

(provide (except-out (all-from-out racket) define set! let letrec require)
         (all-from-out "player.rkt"))

;; ... Other provisions are co-located with their definitions.



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


(provide ask)
(define-syntax (ask stx)
  ;;;;;;;;;;;;  ---
  (syntax-case stx ()
    [(_ player choices)
     #'(let ()
         (display (~a "ask returns " (car choices) "\n"))  ;~~~ todo... need reset/prompt magic.
         (car choices)
         )]))

(provide define/provide-rules)
(define-syntax (define/provide-rules stx)
  ;;;;;;;;;;;;  --------------------
  (syntax-case stx ()
    [(_ rule-id body ...)
     (with-syntax
         ([players (datum->syntax stx 'rules$players)])
       #`(begin
           (provide (contract-out [rule-id (-> (listof player?)  any)]))
           #,(syntax/loc stx
               (define (rule-id players)
                 (let ()
                   (rules-preamble players)
                   body ...
                   (rules-state-posure-parm))))))]))


(provide define-section)
(define-syntax (define-section stx)
  ;;;;;;;;;;;;  --------------------
  (syntax-case stx ()
    [(_ section-id body0 bodyN ...)
     ;; Create a new posure for this section, using the outside one as a parent.
     ;; We used a double-nested let here to aid error-reporting.
     ;; Errors occurring before and after the inner let, are likely bugs in
     ;; the rules-lang.
     
     #`(let ()
         (push-new-empty-posure!)
         (remember $section (symbol->string (quote section-id)))
         #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
             (let ()
               body0 bodyN ...))
         (pop-posure!)
         (void))]))


(define (dollar id-symbol stx)
  ;;;;;  ------
  (posure-$ (rules-state-posure-parm) id-symbol))


(define/provide/contract-out (first-game-of-match?)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------------------
  (-> boolean?)
  (false? ($ previous-game-player-who-decided-who-played-first)))


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
(provide for-each-player-in-parallel)
(define-syntax (for-each-player-in-parallel stx)
  ;;;;;;;;;;;;  ---------------------------
  (syntax-case stx ()
    [(_ rules-var-id body0 bodyN ...)
     (with-syntax
         ([internal-var-id (datum->syntax stx 'rules$internal-var)])
       #`(begin
           (push-new-empty-posure!)
           (remember rules-var-id #f)
           (for ([internal-var-id ($ players)])
             (:= rules-var-id internal-var-id)
             #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
                 (let ()
                   body0 bodyN ...))
             )
           (pop-posure!)
           )
       )
     ]))

(define (pop-posure!)
  ;;;;;  -----------
  (rules-state-posure-parm (posure-parent (rules-state-posure-parm))))

(define/provide/contract-out (previous-game-was-a-draw?)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------------------
  (-> boolean?)
  (false? ($ prior-game-losing-player)))

(define (push-new-empty-posure!)
  ;;;;;  ----------------------
  (rules-state-posure-parm (posure (make-hasheq) (rules-state-posure-parm))))


(define/provide/contract-out (random-player)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------
  (-> player?)
  (car (shuffle ($ players))))


(provide remember)
(define-syntax (remember stx)
  ;;;;;;;;;;;;  --------
  (syntax-case stx ()
    [(_ id val)
     (syntax/loc stx
       (posure-bind-value (rules-state-posure-parm) (quote id) val))
     ]
    ;[(_ id)
    ; (syntax/loc stx
    ;   (error "remember: requires two parameters"))]
    ))

(define/provide/contract-out (rules-preamble player-list)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------------
  (-> list?   any)
  (remember section "root")
  (remember players player-list))

(define/provide/contract-out (shuffle! player zone-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  (-> player? symbol?  void?)
  (set-player-zone! player zone-sym (shuffle (player-zone player zone-sym))))
