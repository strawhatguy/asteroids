(module ship racket
  (require lens)
  (provide (all-defined-out))
  
  (struct/lens ship (loc vel accel angle engine-on) #:transparent)

  (define (fire-thruster s)
    (lens-set ship-engine-on-lens s #t))

  (define (stop-thruster s)
    (lens-set ship-engine-on-lens s #f))

  (define (turn-ship-right s delta)
    (lens-transform ship-angle-lens s (λ (a) (- a delta))))
  
  (define (turn-ship-left s delta)
    (lens-transform ship-angle-lens s (λ (a) (+ a delta))))

  )