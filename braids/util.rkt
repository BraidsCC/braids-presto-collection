#lang racket

;; Copyright 2016 Braids Constance.  All rights reserved.

;; If you are seeing spurious exceptions from this file that might actually originate
;; in your own code, it's probably because this file lacks proper use of
;; (syntax/loc stx ... ).  However, Braids hasn't been able to produce any
;; such problems.  "Odd..."


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros for macros.

;; Order for these seems important, so they are at the top of the file.


;; Convenience macro for define-syntax with provide.
;;
;; Syntax:  
;; (define-syntax-case/provide (head args) body ...+)
;;
;; Example:
;;(define-syntax/provide (guilty-set! stx)
;;  (syntax-case stx ()
;;    [(_ id val)
;;     (begin
;;       (display "You have deviated from the One Truth Path by using (set! ")
;;       (display (quote id))
;;       (display ")\n")
;;       (syntax/loc stx
;;         (set! id val))
;;       )]))
(provide define-syntax/provide)
(define-syntax (define-syntax/provide dsp-stx)
  ;;;;;;;;;;;;  ---------------------
  (syntax-case dsp-stx ()
    [(_ (rator stx0) body0 bodyN ...)
     #`(begin
         (provide rator)
         (define-syntax (rator stx0)
           body0 bodyN ...
           )
         )]))


;; Convenience macro for define-syntax containing a syntax-case with a provide form.
;;
;; Syntax:  
;; (define-syntax-case/provide (head args)
;;   clause ...)
;;
;; Example:
;; (define-syntax-case/provide (guilty-set! stx)
;;   [(_ id val)
;;    (begin
;;      (syntax/loc stx  ;; consumer-oriented error reporting
;;        (let ()
;;          (set! id val)
;;          "You have deviated from the One Truth Path by using set!")))])
(define-syntax/provide (define-syntax-case/provide dscmp-stx)
  ;;;;;;;;;;;;;;;;;;;;  --------------------------

  ;; one macro to rule all macros...
  (syntax-case dscmp-stx ()
    [(_ (rator stx0) match0 matchN ...)
     #`(define-syntax/provide (rator stx0)

         ;; use the outer macro's user's srcloc for exceptions.
         #,(syntax/loc dscmp-stx  
             (syntax-case stx0 ()
               match0 matchN ...)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros and procedures.


;; Despite the name, this doesn't actually mutate the list.
;;
(define-for-syntax (omit-second-item-of-each-sublist ls)
  ;;;;;;;;;;;;;;;;  --------------------------------
  (cond
    [(eq? '() ls)
     '()]
    [else
     (define first-list (car ls))
     (define first-list-sans-second-item
       (cons (car first-list) (cddr first-list)))

     (cons first-list-sans-second-item 
                  (omit-second-item-of-each-sublist (cdr ls)))]))


;; Mutative add1 that returns the resulting number after incrementing.
;;
;; e.g.,
;;
;; (let ([x 7])
;;   (if (eq? 8 (add1! x))
;;       (display "yay\n")
;;       (void)))
;;
;; displays "yay".
;;
;(provide add1!)
(define-syntax/provide (add1! stx)
;(define-syntax (add1! stx)
  ;;;;;;;;;;;;  -----
  (syntax-case stx ()
    [(_ num-var)
     #'(let () (set! num-var (add1 num-var)) num-var)     
     ]))


;; Combine define and provide for procedures.
;; Syntax is intentionally identical to define for most purposes, including
;; values (aka exprs) and procedures.
;;
(provide define/provide)
(define-syntax (define/provide stx)
  ;;;;;;;;;;;;  --------------
 (syntax-case stx ()
   [(_ (rator rands ...) body0 bodyN ...)
    #'(begin
        (provide rator)
        (define (rator rands ... ) body0 bodyN ...))]
   
   [(_ id val)
    #'(begin
        (provide id)
        (define id val))]))


;; Combine define and (provide (contract-out ...)) for procedures.
;; Syntax:
;; (define/provide/contract-out (head args) contract-expr body ...+)
;;
;; e.g.,
;;
;; (define/provide/contract-out (println str)
;;   (-> string?   void?)
;;
;;   (display str)
;;   (newline))
;;   
(define-syntax-case/provide (define/provide/contract-out stx)
  ;;;;;;;;;;;;;;;;;;;;;;;;;  ---------------------------
    [(_ (rator rands ...) (tract ...) body0 bodyN ...)
     #`(begin
         (provide (contract-out [rator (tract ...)]))
         #,(syntax/loc stx
             (define (rator rands ...) body0 bodyN ...)))]
    );/define-syntax d/p/co


;; Invokes hash-ref! with the resulting value in tail position.
;;
(define/provide/contract-out (hash-ref!/tail hash key value-thunk)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;  --------------
  (-> (and/c hash? (not/c immutable?))  ;hash
      any/c  ;key
      (-> any/c)  ;value-thunk
      any)

  (define computed-value (void))

  ;; computed-value is only mutated if this thunk executes.
  (define (value-thunk-prime)
    (set! computed-value (value-thunk)))

  ;; hash-ref! decides whether to execute the thunk.
  (define hash-ref!-result
    (hash-ref! hash key value-thunk-prime))

  (if (not (void? computed-value))
      computed-value
      hash-ref!-result
      )
  );def



;; Returns a predicate that determines whether a specific value is a member of
;; the given set.
;;
(define/provide (set-member/c st)
  ;;;;;;;;;;;;;  ------------

  ;(-> set?   (-> any/c   boolean?))

  (flat-named-contract
   (~a "member of set " st)
   (lambda (val)
     (set-member? st val))))



;; Combine struct and (provide (struct-out ...)) for structs.
;; Syntax is just like struct (in Racket 6.5).
;;
(provide struct/provide)
(define-syntax (struct/provide stx)
  ;;;;;;;;;;;;  --------------
  (syntax-case stx ()
    [(_ id (fields ...) options ...)
     #'(begin
         (provide (struct-out id))
         (struct id (fields ...) options ...))]
    
    [(_ id super-struct (fields ...) options ...)
     #'(begin
         (provide (struct-out id))
         (struct id super-struct (fields ...) options ...))]
    );syn case
  );def


;; Combine struct and (provide (contract-out ... )) for structs.
;; Syntax:
;; (struct/provide/contract-out id maybe-super
;;      ([field-id field-contract field-option ...] ...)
;;      struct-option ...)
;;
;; e.g.,
;;
;; (struct/provide/contract-out flowering-plant plant
;;   ([pleasantly-scented boolean?]
;;    [petal-count exact-nonnegative-integer? #:mutable])
;;   #:transparent)
;;
(provide struct/provide/contract-out)
(define-syntax (struct/provide/contract-out stx)
  ;;;;;;;;;;;;  ---------------------------
  (syntax-case stx ()
   [(_ id (field ...) options ...)
    (let ()
      (define field-datums (syntax->datum #'(field ...)))
      ; field-datums might look like '((f1 number?) (f2 boolean? #:mutable #:auto)).
      ; Convert to '((f1) (f2 #:mutable #:auto)) for struct
      ; and to '((f1 number?) (f2 boolean?)) for contract-out
      (define struct-field-datums (omit-second-item-of-each-sublist field-datums))
      (define contract-datums (truncate-sublists-to-two-items field-datums))
      
      (with-syntax
          ([struct-fields (datum->syntax stx struct-field-datums)]
           [contracts (datum->syntax stx contract-datums)])
        #'(begin
            (provide (contract-out
                      [struct id contracts]))
            (struct id struct-fields options ...)
            );begin
      ) ;with-syntax
      );let
    ]

   ; experimental
   [(_ id super-struct (field ...) options ...)
    (let ()
      (define field-datums (syntax->datum #'(field ...)))
      (define struct-field-datums (omit-second-item-of-each-sublist field-datums))
      (define contract-datums (truncate-sublists-to-two-items field-datums))
      
      (with-syntax
          ([struct-fields (datum->syntax stx struct-field-datums)]
           [contracts (datum->syntax stx contract-datums)])
        #'(begin
            (provide (contract-out
                      [struct id contracts]))
            (struct id super-struct struct-fields options ...)
            );begin
        ) ;with-syntax
      );let
    ]
   );/syntax-case
  );/def


;; Returns a syntax object that represents the srcloc (source location) of
;; another (given) syntax object, using the original syntax-context of the
;; given syntax object.
(define (syntax->srcloc->syntax-@DEPRECATED stx)
  ;;;;;  ----------------------------------
  (datum->syntax stx
   (srcloc (syntax-source stx)
           (syntax-line stx)
           (syntax-column stx)
           (syntax-position stx)
           (syntax-span stx))))


(define/provide the-empty-hash (hash))  ; immutable
;;;;;;;;;;;;;;; --------------


;; Despite the name, this doesn't actually mutate the list.
;;
(define-for-syntax (truncate-sublists-to-two-items ls)
  ;;;;;;;;;;;;;;;;  ------------------------------
  (cond
    [(eq? '() ls)
     '()]
    [else
     (define first-list (car ls))
     (define first-list-first-2-only (list (car first-list) (cadr first-list)))
     
     (cons first-list-first-2-only
            (truncate-sublists-to-two-items (cdr ls)))
     ]
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graveyard...

;(define-for-syntax (syntactic-map stx proc data-from-syntax)
;                    /////////////
;  (datum->syntax stx (map proc data-from-syntax)))

;(define-for-syntax (syntactic-cars stx data-from-syntax)
;                    //////////////
;  (syntactic-map stx car data-from-syntax))

;(define/provide/contract-out (mutable? value)
;                             /////////
;  (-> any/c
;      boolean?)
;  (not (immutable? value)))


