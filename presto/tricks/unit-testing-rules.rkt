#lang s-exp "tricks.rkt"


(define/provide-rules (unit-testing-rules players external-k)
  ;(display (~a "start posure: " (rules-state-posure-parm) "\n"))
  
  (remember assert (lambda (boo) (cond [(false? boo) (error "assertion failed")])))

  
  
  (remember root-cause #f)
  (remember overridden 'please)

  (($ assert) (not ($ root-cause)))
  (($ assert) (eq? ($ overridden) 'please))

  
  ;(display (~a "pre-one posure: " (rules-state-posure-parm) "\n"))
  
  (define-section one
    (remember onsie 1)
    (remember overridden 'splat)
    (:= root-cause "evil")

    (($ assert) (eq? ($ onsie) 1))
    (($ assert) (eq? ($ overridden) 'splat))
    (($ assert) (eq? ($ root-cause) "evil"))

    (add1 ($ onsie))
    
    ;(display (~a "one end posure: " (rules-state-posure-parm) "\n"))
    );def-sec

  ;(display (~a "post-one posure: " (rules-state-posure-parm) "\n"))

  (($ assert) (eq? ($ overridden) 'please))
  (($ assert) (eq? ($ root-cause) "evil"))
  
  ;(display (~a "end posure: " (rules-state-posure-parm) "\n"))

  (remember question '(house-salad caesar-salad))
  
  (ask ($ external-k) player ($ question))
  );def-rules
