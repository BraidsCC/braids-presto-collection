#lang s-exp "tricks.rkt"


(define (assert boo message)
  (cond [(false? boo)
         (error (~a "assertion failed: " message))]
        
        [(not (boolean? boo))
         (error (~a "not a boolean: " boo " for " message))])
  (void))



(define/provide-rules unit-testing-rules

  (define root-cause #f)
  (define overridden 'please)

  (assert (not root-cause) "root cause is false")
  (assert (eq? overridden 'please) "overridden is 'please")

    
  (define-section one
    (define onesie 1)
    (define overridden 'splat)
    (set! root-cause "evil")
    
    (assert (eq? onesie 1) "onesie is 1")
    (assert (eq? overridden 'splat) "overridden is 'splat")
    (assert (eq? root-cause "evil") "root-cause is \"evil\"")
    
    (add1 onesie)
    
    );def-sec
  
  (assert (eq? overridden 'please) "overridden is back to being 'please")
  (assert (eq? root-cause "evil") "root-cause is still \"evil\"")
  
  (define options '(house-salad caesar-salad))
  
  (for-each-player-in-parallel that-player 
    (ask that-player options))
    
  (set! options '(beef pork chicken vegetables))
  (ask (car (players-parm)) options)


  );def-rules
