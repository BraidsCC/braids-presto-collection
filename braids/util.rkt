#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures needed by syntax-extensions.


;; Despite the name, this doesn't actually mutate the list.
;;
(define-for-syntax (omit-second-item-of-each-sublist ls)
  (cond
    [(eq? '() ls)
     '()]
    [else
     (define first-list (car ls))
     (define first-list-sans-second-item
       (cons (car first-list) (cddr first-list)))

     (cons first-list-sans-second-item 
                  (omit-second-item-of-each-sublist (cdr ls)))]
    ))

;; Returns a syntax object that represents the srcloc (source location) of
;; another (given) syntax object, using the original syntax-context of the
;; given syntax object.
(provide syntax->srcloc->syntax)
(define (syntax->srcloc->syntax stx)
  (datum->syntax stx
   (srcloc (syntax-source stx)
           (syntax-line stx)
           (syntax-column stx)
           (syntax-position stx)
           (syntax-span stx))))

;; Despite the name, this doesn't actually mutate the list.
;;
(define-for-syntax (truncate-sublists-to-two-items ls)
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
;; Syntax extensions


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
(define-syntax (add1! stx)
  ;;;;;;;;;;;;
  (syntax-case stx ()
    [(_ num-var)
     #'(let () (set! num-var (add1 num-var)) num-var)     
     ]))
(provide add1!)


;; Combine define and provide for procedures.
;; Syntax is intentionally identical to define for most purposes, including
;; values (aka exprs) and procedures.
;;
(define-syntax (define/provide stx)
  ;;;;;;;;;;;;
 (syntax-case stx ()
   [(_ (rator rands ...) body0 bodyN ...)
    #'(begin
        (provide rator)
        (define (rator rands ... ) body0 bodyN ...))]
   
   [(_ id val)
    #'(begin
        (provide id)
        (define id val))]))
(provide define/provide)


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
(define-syntax (define/provide/contract-out stx)
  ;;;;;;;;;;;;
  (syntax-case stx ()
    [(_ (rator rands ...) (tract ...) body0 bodyN ...)
     #'(begin
         (provide (contract-out [rator (tract ...)]))
         (define (rator rands ...) body0 bodyN ...))]
    ));/define-syntax d/p/co
(provide define/provide/contract-out)


;; Combine struct and (provide (struct-out ...)) for structs.
;; Syntax is just like struct (in Racket 6.5).
;;
(define-syntax (struct/provide stx)
  ;;;;;;;;;;;;
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
(provide struct/provide)


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
(define-syntax (struct/provide/contract-out stx)
  ;;;;;;;;;;;;
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
(provide struct/provide/contract-out)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define/provide the-empty-hash (hash))  ; immutable


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures


;(define/provide/contract-out (mutable? value)
;  (-> any/c
;      boolean?)
;  (not (immutable? value)))


;; Invokes hash-ref! with the resulting value in tail position.
;;
(define/provide/contract-out (hash-ref!/tail hash key value-thunk)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graveyard...

;(define-for-syntax (syntactic-map stx proc data-from-syntax)
;  (datum->syntax stx (map proc data-from-syntax)))
;
;(define-for-syntax (syntactic-cars stx data-from-syntax)
;  (syntactic-map stx car data-from-syntax))

