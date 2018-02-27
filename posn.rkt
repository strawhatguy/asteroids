(module posn racket
  (require lens)
  (provide (all-defined-out))
  
  (struct/lens posn (x y) #:transparent)

  (define (posn=? p1 p2)
    (and (= (posn-x p1) (posn-x p2))
         (= (posn-y p1) (posn-y p2))))

  (define (posn+ p1 p2)
    (posn (+ (posn-x p1) (posn-x p2))
          (+ (posn-y p1) (posn-y p2))))

  (define (posn* p1 p2)
    (posn (* (posn-x p1) (posn-x p2))
          (* (posn-y p1) (posn-y p2))))

  (define (posn-dist-sqr p)
    (+ (sqr (posn-x p)) (sqr (posn-y p))))

  (define (posn-dist p)
    (sqrt (posn-dist-sqr p)))

  (define (posn-rot p angle)
    (define cos-angle (cos (degrees->radians angle)))
    (define sin-angle (sin (degrees->radians angle)))
    (posn (+ (* cos-angle (posn-x p))
             (* sin-angle (posn-y p)))
          (+ (* -1 sin-angle (posn-x p))
             (* cos-angle (posn-y p)))))

  (define (random-posn boundx boundy)
    (posn (random (max boundx 1))
          (random (max boundy 1))))
  
  (module* test #f
    (require rackunit)
    (check-equal? (posn+ (posn 1 2) (posn 3 4)) (posn 4 6))
    (check-equal? (posn-rot (posn 1 0) 0) (posn 1 0)))
  )