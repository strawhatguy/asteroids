#lang racket
(module asteroid racket
(require 2htdp/universe 2htdp/image)
(require lens)
(require "posn.rkt")
(require "bullet.rkt")
(require "ship.rkt")
(require "asteroid.rkt")
(require "physics.rkt")

(define TICK-RATE 1/40)
(define SIZE 20)
(define WIDTH-PX  (* SIZE 30))
(define HEIGHT-PX (* SIZE 30))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define ROTATION-DEG-PER-TICK (* 180 2 TICK-RATE))
(define SHIP-IMG (overlay (isosceles-triangle SIZE 40 "solid" "gray")
                          (square SIZE 0 "white")))
(define SHIP-ACCEL-RATE 160)
(define BULLET-IMG (circle 2 "outline" "black"))
(define SHIP-MAX-VELOCITY 100)

(struct/lens asteroid-field (ship bullets asteroids) #:transparent)

(define (start)
  (big-bang (asteroid-field (new-ship)
                            empty
                            (new-asteroids 8 WIDTH-PX HEIGHT-PX (- 5 ASTEROID-START-VEL) ASTEROID-START-VEL))
    [on-tick update-field TICK-RATE]
    [on-key handle-key-down]
    [on-release handle-key-up]
    [to-draw render-field WIDTH-PX HEIGHT-PX]))

(define (render-ship s)
  (rotate (ship-angle s) SHIP-IMG))

(define (update-field w)
  (lens-transform/list w
                       asteroid-field-ship-lens next-ship
                       asteroid-field-bullets-lens next-bullets
                       asteroid-field-asteroids-lens next-asteroids))

(define (handle-key-down w k)
  (cond [(key=? k "up") (world-change-ship fire-thruster w)]
        [(key=? k "right") (world-change-ship (curryr turn-ship-right ROTATION-DEG-PER-TICK) w)]
        [(key=? k "left") (world-change-ship (curryr turn-ship-left ROTATION-DEG-PER-TICK) w)]
        [(key=? k "down") (world-fire-bullet w)]
        [else w]))

(define (world-fire-bullet w)
  (define s (asteroid-field-ship w))
  (lens-transform asteroid-field-bullets-lens w
                  (λ (bs)
                    (cons (new-bullet (ship-loc s) (ship-angle s))
                         bs))))

(define (handle-key-up w k)
  (cond [(key=? k "up") (world-change-ship stop-thruster w)]
        [else w]))

(define (world-change-ship action w)
  (lens-transform asteroid-field-ship-lens w action))

(define (render-field w)
  (bullets+scene (asteroid-field-bullets w)
                 (asteroids+scene (asteroid-field-asteroids w)
                                  (ship+scene (asteroid-field-ship w) MT-SCENE))))

(define (bullets+scene bs scene)
  (foldl bullet+scene scene bs))

(define (bullet+scene b scene)
  (define loc (bullet-loc b))
  (place-image BULLET-IMG (posn-x loc) (posn-y loc) scene))

(define (ship+scene s scene)
  (define loc (ship-loc s))
  (place-image (render-ship s)
               (posn-x loc) (posn-y loc)
               scene))

(define (asteroids+scene as scene)
  (foldl asteroid+scene scene as))

(define (asteroid+scene a scene)
  (define loc (asteroid-loc a))
  (place-image (asteroid-img (asteroid-size a)) (posn-x loc) (posn-y loc) scene))

(define (asteroid-img size)
  (radial-star (+ size 8) (- size 2) size "outline" "black"))

(define (new-ship)
  (ship (posn (/ WIDTH-PX 2) (/ HEIGHT-PX 2))
        (posn 0 0)
        (posn 0 0)
        0
        #f))

(define (next-loc loc vel accel)
  (posn (pos-equation (posn-x loc) (posn-x vel) (posn-x accel) TICK-RATE)
        (pos-equation (posn-y loc) (posn-y vel) (posn-y accel) TICK-RATE)))
(define (next-vel vel accel)
  (posn (vel-equation (posn-x vel) (posn-x accel) TICK-RATE)
        (vel-equation (posn-y vel) (posn-y accel) TICK-RATE)))

(define (maybe-loop-bounds p)
  (define (coord-adjust c maxbound)
    (cond [(< c 0) (+ c maxbound)]
          [(> c maxbound) (- c maxbound)]
          [else c]))
  (define (coord-adjust-width c) (coord-adjust c WIDTH-PX))
  (define (coord-adjust-height c) (coord-adjust c HEIGHT-PX))
  (lens-transform/list p posn-x-lens coord-adjust-width
                       posn-y-lens coord-adjust-height))

(define (next-ship s)
  (struct-copy ship s
               [loc (maybe-loop-bounds (next-loc (ship-loc s) (ship-vel s) (ship-accel s)))]
               [vel (next-vel (ship-vel s) (ship-accel s))]
               [accel (if (ship-engine-on s)
                          (posn-rot (posn 0 (- SHIP-ACCEL-RATE)) (ship-angle s))
                          (posn 0 0))]))
(define (next-bullets bs)
  (filter (λ (b) (not (zero? (bullet-ticks b)))) (map next-bullet bs)))

(define (next-bullet b)
  (struct-copy bullet b
               [loc (maybe-loop-bounds (next-loc (bullet-loc b) (bullet-vel b) (posn 0 0)))]
               [ticks (sub1 (bullet-ticks b))]))

(define (next-asteroids as)
  (map next-asteroid as))

(define (next-asteroid a)
  (struct-copy asteroid a
               [loc (maybe-loop-bounds (next-loc (asteroid-loc a) (asteroid-vel a) (posn 0 0)))]
               [vel (next-vel (asteroid-vel a) (posn 0 0))]))

(module* test #f
  (require rackunit)
  (check-equal? (next-ship (new-ship)) (new-ship))
  (check-equal? (maybe-loop-bounds (posn -1 (add1 HEIGHT-PX))) (posn (sub1 WIDTH-PX) 1))
  (check-equal? (maybe-loop-bounds (posn 1 2)) (posn 1 2)))

