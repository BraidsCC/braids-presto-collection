#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #lang module for tricks, the rules language for presto.
;;
;; For a detailed example of how to use the language, see
;; "unit-testing-rules.rkt."


(require (for-syntax racket/syntax))
(require (for-syntax "../../braids/util.rkt"))

(require "../../braids/util.rkt")
(require "../player.rkt")
(require "question.rkt")
(require "answer.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source-level configuration options.



;; Development option; disabling may enhance performance.
(define/provide tricks-debug-flag #t)
;;;;;;;;;;;;;;; -----------------



;; Provide nearly all of Racket, except discourage changing values
;; inside data structures.  Regular /set!/ is OK, though.  (this is because i'm
;; a tad superstitious when it comes to time-travel using continuations).
;;
(provide (except-out (all-from-out racket)
                     box-cas! bytes-copy! bytes-fill! bytes-set!
                     dict-clear! dict-ref! dict-remove! dict-set!
                     dict-set*! dict-update! ;racket 6.5 can't find: dynamic-enter!
                     dynamic-set-field! ;not found: enter!
                     environment-variables-set!
                     ;not found: extflvector-set! flvector-set! fxvector-set!
                     hash-clear! hash-ref! hash-remove! hash-set! hash-set*!
                     ;not found: hash-union!
                     hash-update!
                     namespace-set-variable-value! namespace-undefine-variable!
                     peek-bytes! ;not found: peek-bytes-avail! peek-bytes-avail*!
                     peek-string! placeholder-set! plumber-add-flush!
                     ;not found: plumber-flush-update-handle-remove!
                     port-count-lines!
                     read-bytes! read-bytes-avail! read-bytes-avail!*
                     read-string! set-add! set-box! 
                     set-clear! set-field! set-intersect!
                     set-mcar! set-mcdr! set-phantom-bytes!
                     set-port-next-location! set-remove! set-subtract!
                     set-symmetric-difference! set-union!
                     string-copy! string-fill! string-set!
                     thread-cell-set! ;not found: vector->psuedo-random-generator!
                     vector-copy! vector-fill! vector-map! vector-set!
                     vector-set*! vector-set-performance-stats!)
         (all-from-out "../player.rkt"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct-use parameters.



(define/provide players-parm (make-parameter '()))
;;;;;;;;;;;;;;; ------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct-use procedures and macros.
;;
;; This block contains macros and procedures that one would normally use in
;; Tricks rules-definitions.



;; Build a question struct to get back an answer struct.
(define/provide/contract-out (ask player choices)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ---  

  (->i ([player presto-player?]
        [choices (and/c set? (not/c set-empty?))])
       [result (choices) (set-member/c choices)])

  (let* ([controller-k  (controller-k-parm)]
         [answer  (ask-helper player choices controller-k)])
    (controller-k-parm (tricks-answer-controller-k answer)) ;set parm
    (tricks-answer-choice answer) ;return player's choice
    ))


;; Select an option from a tricks-question.
;;
;; This enacts that option and saves the resulting state in a new tricks-question, which represents the next decision point in the rules definition.
;;
;; Returns the new tricks-question.
;;
(define/provide/contract-out (choose-for question-in player selected-val)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ----------
  
  (-> tricks-question? presto-player? any/c  tricks-question?)
  
  ;; Counterpart to ask:  build an answer struct to get the next question struct.
  (let*
      ([rules-k (tricks-question-rules-k question-in)]
       [question-out (let/cc controller-k
                       (rules-k (tricks-answer controller-k player selected-val)))])

    ;~~~ change the following to use debug-monitor-contract
    (cond [(tricks-debug-flag . and . (not (tricks-question? question-out)))
           (define message (~a "choose (via continuation): contract violation\n"
                               " expected: tricks-question?\n"
                               " given:\n"
                               "   " question-out))
           (raise (make-exn:fail:contract message (current-continuation-marks)))])
    question-out))



(define-syntax-case/provide (define/provide-rules stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  --------------------
  [(_ rules-id body0 bodyN ...)
   ; =>
   ;; Do not invoke rules-id directly; instead, use start-game.
   #`(define/provide/contract-out (rules-id players-formal controller-k-formal)
       (-> (listof presto-player?) continuation?   any)

       ; Set parameters.
       (players-parm players-formal)
       (controller-k-parm controller-k-formal)
       (section-stack-parm '(root))
       
       #,(syntax/loc stx
           (let ()
             body0 bodyN ...))
       "game ended")])



(define-syntax-case/provide (define-section stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  --------------
  [(_ section-id body0 bodyN ...)
   ; =>
   ;; The double-nested let here aids error-reporting.
   ;; Errors occurring before and after the inner let, are likely bugs in
   ;; Tricks, which is why we used syntax/loc for the body.
   #`(let ([section-stack  (section-stack-parm)])
       (section-stack-parm (cons (quote section-id) section-stack)) ;push

       #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
           (let ()
             body0 bodyN ...))
       
       (section-stack-parm (cdr section-stack)) ;pop
       )])



;~~~ move to Presto's rules-definition.
;(define/provide/contract-out (first-game-of-match?)
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------------------
;  (-> boolean?)
;  (false? ($ previous-game-player-who-decided-who-played-first)))



;; Loops over all players in any order. While not yet implemented as an
;; asynchronous operation, it's a cue should that ever become desirable.
;;
;; Syntax:
;; (for-each-player-in-parallel variable-name
;;   body+ ...)
;;
;; Example:
;; (for-each-player-in-parallel that-player
;;   (shuffle! that-player 'library))
;;
(define-syntax-case/provide (for-each-player-in-parallel stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---------------------------
  [(_ internal-var-id body0 bodyN ...)
     
   #`(let ([players (players-parm)])
       (for ([internal-var-id players])
         #,(syntax/loc stx  ;; Transfer error reporting to consumer code.
             (let ()
               body0 bodyN ...))))])



;~~~ move to Prestos' rules def.
;(define/provide/contract-out (previous-game-was-a-draw?)
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------------------
;  (-> boolean?)
;  (false? ($ prior-game-losing-player)))



(define/provide/contract-out (random-player)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  -------------

  (-> presto-player?)
  (car (shuffle (players-parm))))



(define/provide/contract-out (shuffle! player zone-sym)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------
  (-> presto-player? symbol?  void?)
  (set-player-zone! player zone-sym (shuffle (player-zone player zone-sym))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOR OFFICIAL USE ONLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; These forms are not intended for direct use
;; in rules-definitions, but are necessary for them to work and/or for other
;; code to use them.
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables (parameters).



(define controller-k-parm (make-parameter #f))
;;;;;;; ---------------



(define section-stack-parm (make-parameter '()))
;;;;;;; ------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros and procedures.



(define (ask-helper player question controller-k)
  ;;;;;  --------
  ;(-> presto-player? list? continuation?   tricks-answer?)

  (let
      ([answer
        (let/cc rules-k
          (controller-k (tricks-question rules-k player question)))])

    (debug-monitor-contract answer tricks-answer? "ask (via continuation)")
    answer))



(define (debug-monitor-contract val contract source-string)
  ;;;;;  ----------------------

  ;(-> any/c contract? string?   any)

  (cond [(tricks-debug-flag . and . (not (contract val)))
         (define message (~a source-string ": contract violation\n"
                             " expected: " contract "\n"
                             " given:\n"
                             "   " val))
         (raise (make-exn:fail:contract message (current-continuation-marks)))]))




(define/provide/contract-out (start-game rules players)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  ----------

  (-> procedure? (listof presto-player?)  tricks-question?)

  (let/cc k (rules players k)))
