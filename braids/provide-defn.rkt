#lang racket

;; Copyright 2016 Braids Constance.

(require (for-syntax racket/match))
(require (for-syntax racket/format))

(require rackunit)

(begin-for-syntax

  (define (generate-struct-list id supah fields options) ; --> list?
    ;; For explanations of the parameters, see struct.
    ;; This returns a list-and-symbol form that declares a struct.
    ;;

    (append
     (list 'struct id)
     (if (void? supah)
         '()
         (list supah))
     (list fields)
     options))
  
  (define (generate-struct/provide id supah fields options) ; --> list?
    ;; For explanations of the parameters, see struct.
    ;;
    ;; This returns a list-and-symbol form that provides and declares a struct.
    ;; 

    (list 'begin (list 'provide (list 'struct-out id))
          (generate-struct-list id supah fields options)))

  (define (generate-define/provide-proc-form rator rands body-exprs)
    (list 'begin (list 'provide rator) (append (list 'define (cons rator rands))
                                               body-exprs)))

  (define (do-match input) ; --> any
    ;; input : any
    ;;
    ;; Returns a list-based, prototypical syntactic expansion of input.
    ;;
    ;; This function implements list-and-symbol versions (not syntax versions) of
    ;; define/provide and struct/provide.  If input starts with either of these symbols,
    ;; the result starts with a begin form that provides and defines the
    ;; id following the provide-symbol.
    
    (match input
      [(list-rest 'define/provide (list-rest rator rands) body-exprs)
       (generate-define/provide-proc-form rator rands body-exprs)]
      [(list 'define/provide name value)
       (list 'begin (list 'provide name) (list 'define name value))]
      [(list-rest 'struct/provide id (list-rest fields) options)
       (generate-struct/provide id (void) fields options)]
      [(list-rest 'struct/provide id supah (list-rest fields) options)
       (generate-struct/provide id supah fields options)]
      [(and _ non-matching-expr)
       (~a "error: " non-matching-expr " is illegal for define/provide and/or struct/provide")]))
    
  ) ; end-for-syntax


#;(begin

(define (display-match input) ; --> void?
  ;; input : any
  ;;
  ;; Performs do-match on the input, then displays it, followed by a newline.

  (display (do-match input))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These unit-tests also serve as examples for do-match.
;; The format is
;; (check-equal? (do-match example-input) example-output)
;;

(check-equal? (do-match 'blah)  ; non-matching-expr
              'blah)

(check-equal? (do-match '(define/provide x 42))
              '(begin
                 (provide x)
                 (define x 42)))

(check-equal? (do-match '(define/provide (proc1 x) (+ x 1)))
              '(begin
                 (provide proc1)
                 (define (proc1 x) (+ x 1))))
              
(check-equal? (do-match '(define/provide (proc2 x y z) do-stuff (if x y z)))
              '(begin
                 (provide proc2)
                 (define (proc2 x y z) do-stuff (if x y z))))

(check-equal?
 (do-match '(struct/provide mystruct1 superstructure (field1 field2) option1 option2))
 '(begin
    (provide (struct-out mystruct1))
    (struct mystruct1 superstructure (field1 field2) option1 option2)))

(check-equal? (do-match '(struct/provide mystruct-simple ()))
              '(begin
                 (provide (struct-out mystruct-simple))
                 (struct mystruct-simple ())))
) ;end-begin-that-is-commented-out


;; Notes to self about macro quoting:
;; `(foo bar) means plays a literal (foo bar) in the output.
;; `( ... ,(rator rand1 rand2 ...)) calls rator with rand1 and rand2,
;; then place the result into the output symbolically.  (,() allows us to
;; call macro helpers from within macro definitions.)

(define-syntax (define/provide-v2 stx)
  (match (syntax->datum stx)
    [(list-rest 'define/provide (list-rest rator rands) body-exprs)
     (datum->syntax
      stx
      `(begin
         (provide ,rator)
         ;;,(append (list 'define (cons rator rands)) body-exprs)
         ,(cons 'define (cons (cons rator rands) body-exprs))
         ))]

    [(list 'define/provide name value)
     (datum->syntax stx `(begin (provide ,name) (define ,name ,value)))]))

(define-syntax (struct/provide-v2 stx)
  (datum->syntax stx
   (match (syntax->datum stx)
     [(list-rest 'struct/provide id (list-rest fields) options)
      (generate-struct/provide id (void) fields options)]
     [(list-rest 'struct/provide id supah (list-rest fields) options)
      (generate-struct/provide id supah fields options)])))

(define-syntax (define/provide stx)
  (syntax-case stx ()
    [(_ (rator rands ...) body0 bodyN ...)
     #'(begin
         (provide rator)
         (define (rator rands ... ) body0 bodyN ...))]
    
    [(_ id val)
     #'(begin
         (provide id)
         (define id val))]))

(define-syntax (struct/provide stx)
  (syntax-case stx ()
    [(_ id (fields ...) options ...)
     #'(begin
         (provide (struct-out id))
         (struct id (fields ...) options ...))]

    [(_ id super-struct (fields ...) options ...)
     #'(begin
         (provide (struct-out id))
         (struct id super-struct (fields ...) options ...))]))

;;(define/provide x 42)
;;(define/provide (proc1 x) (+ x 1))
;;(define/provide (proc2 x y z) 4 (if x y z))
;;(struct/provide mystruct-simple ())
;;(struct/provide mystruct1 mystruct-simple (field1 field2) #:transparent)

