#lang s-exp "rules-lang.rkt"

(define/provide-rules test-rules
  (display (~a "start posure: " state-posure "\n"))
  
  (remember assert (lambda (boo) (cond [(false? boo) (error "assertion failed")])))
  
  (remember root-cause #f)
  (remember overridden 'please)

  (($ assert) (not ($ root-cause)))
  (($ assert) (eq? ($ overridden) 'please))

  
  (display (~a "pre-one posure: " state-posure "\n"))
  
  (define-section one
    (remember onsie 1)
    (remember overridden 'splat)
    (:= root-cause "evil")

    (($ assert) (eq? ($ onsie) 1))
    (($ assert) (eq? ($ overridden) 'splat))
    (($ assert) (eq? ($ root-cause) "evil"))

    (add1 ($ onsie))
    
    (display (~a "one end posure: " state-posure "\n"))
    );def-sec

  (display (~a "post-one posure: " state-posure "\n"))

  (($ assert) (eq? ($ overridden) 'please))
  (($ assert) (eq? ($ root-cause) "evil"))

  ;; What happens if I reference a variable that DNE?
  ;($ onsie)
  ($ 1)
  
  
  (display (~a "end posure: " state-posure "\n"))
  );def-rules


(test-rules)
