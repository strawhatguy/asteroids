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

  (define (normalize-posn p)
    (define inv-d (/ (posn-dist p)))
    (posn* p (posn inv-d inv-d)))

  (define (posn-rot p angle)
    (define cos-angle (cos (degrees->radians angle)))
    (define sin-angle (sin (degrees->radians angle)))
    (posn (+ (* cos-angle (posn-x p))
             (* sin-angle (posn-y p)))
          (- (* cos-angle (posn-y p))
             (* sin-angle (posn-x p)))))

  (define (random-posn boundx boundy)
    (posn (random (max boundx 1))
          (random (max boundy 1))))
  
  (define (posn-min p dist)
    (define d (min dist (posn-dist p)))
    (define n (normalize-posn p))
    (posn* p (posn d d)))

  (module* test #f
    (require rackunit)
    (check-equal? (posn+ (posn 1 2) (posn 3 4)) (posn 4 6))

    (check-equal? (posn-rot (posn 1 0) 0) (posn 1 0))

    (check-equal? (normalize-posn (posn 1 0)) (posn 1 0))
    (check-equal? (normalize-posn (posn 3 4)) (posn 3/5 4/5))

    (check-equal? (posn-min (posn 5 5) 1/2) (posn 1/2 0))
    )
  )
