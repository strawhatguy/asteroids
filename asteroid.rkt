(module asteroid racket
  (require lens)
  (require "posn.rkt")
  (provide (all-defined-out))

  (define ASTEROID-START-SIZE 30)
  (define ASTEROID-START-VEL 20)
  (struct/lens asteroid (loc vel size) #:transparent)

  (define (new-asteroids count boundx boundy minv maxv)
    (build-list count (λ (_) (new-random-asteroid boundx boundy minv maxv))))

  (define (new-random-asteroid boundx boundy minv maxv)
    (define rando-loc (random-posn boundx boundy))
    (define side (case (random 4)
                   [(0) (lens-set posn-x-lens rando-loc 0)]
                   [(1) (lens-set posn-y-lens rando-loc 0)]
                   [(2) (lens-set posn-x-lens rando-loc boundx)]
                   [(3) (lens-set posn-y-lens rando-loc boundy)]))
    (asteroid side (posn+ (posn minv minv)
                          (posn (random maxv) (random maxv)))
              ASTEROID-START-SIZE))

  (define (split-asteroid a)
    (define twice-vel (posn* (asteroid-vel a) (posn 2 2)))
    (define half-size (/ (asteroid-size a) 2))
    (define a-vel (posn-rot twice-vel (random 360)))
    (define b-vel (posn-rot twice-vel (random 360)))
    (list (asteroid (asteroid-loc a) a-vel half-size)
          (asteroid (asteroid-loc a) b-vel half-size)))

  (module* test #f
    (require rackunit)
    (define demo-ast (new-random-asteroid 100 100 5 10))
    (define split-asts (split-asteroid demo-ast))
    (check-equal? (asteroid-loc demo-ast) (asteroid-loc (first split-asts)))
    (check-= (* 2 (posn-dist (asteroid-vel demo-ast))) (posn-dist (asteroid-vel (first split-asts)))
             0.002)
    (check-= (* 2 (posn-dist (asteroid-vel demo-ast))) (posn-dist (asteroid-vel (second split-asts)))
             0.002))


  )
