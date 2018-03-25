(module bullet racket
  (require lens)
  (require "posn.rkt")
  (provide (all-defined-out))
  
  (define BULLET-VELOCITY (posn 0 -175))
  (define BULLET-LIFETIME-TICKS 125)

  (struct/lens bullet (loc vel ticks) #:transparent)

  (define (new-bullet loc angle)
    (bullet loc (posn-rot BULLET-VELOCITY angle) BULLET-LIFETIME-TICKS))
  )
