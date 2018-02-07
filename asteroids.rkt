#lang racket
(require 2htdp/universe 2htdp/image)
(require lens)

(define TICK-RATE 1/40)
(define SIZE 20)
(define WIDTH-PX  (* SIZE 30))
(define HEIGHT-PX (* SIZE 30))
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define ROTATION-DEG-PER-TICK (* 180 TICK-RATE))
(define SHIP-IMG (overlay (isosceles-triangle SIZE 40 "solid" "gray")
                          (square SIZE 0 "white")))
(define SHIP-ACCEL-RATE 40)

(module+ test
  (require rackunit))

(struct/lens asteroid-field (ship bullets asteroids) #:transparent)
(struct/lens ship (loc vel accel angle engine-on) #:transparent)
(struct/lens posn (x y) #:transparent)

(define (start)
  (big-bang (asteroid-field (new-ship) empty empty)
    [on-tick update-field TICK-RATE]
    [on-key handle-key-down]
    [on-release handle-key-up]
    [to-draw render-field WIDTH-PX HEIGHT-PX]))

(define (update-field w)
  (lens-transform asteroid-field-ship-lens w next-ship))

(define (handle-key-down w k)
  (cond [(key=? k "up") (world-change-ship fire-thruster w)]
        [(key=? k "right") (world-change-ship turn-ship-right w)]
        [(key=? k "left") (world-change-ship turn-ship-left w)]
        [else w]))

(define (handle-key-up w k)
  (cond [(key=? k "up") (world-change-ship stop-thruster w)]
        [else w]))

(define (world-change-ship action w)
  (lens-transform asteroid-field-ship-lens w action))

(define (fire-thruster s)
  (lens-set ship-engine-on-lens s #t))

(define (stop-thruster s)
  (lens-set ship-engine-on-lens s #f))

(define (turn-ship-right s)
  (lens-transform ship-angle-lens s (λ (a) (- a ROTATION-DEG-PER-TICK))))
(define (turn-ship-left s)
  (lens-transform ship-angle-lens s (λ (a) (+ a ROTATION-DEG-PER-TICK))))

(define (render-field w)
  (ship+scene (asteroid-field-ship w) MT-SCENE))

(define (ship+scene s scene)
  (define loc (ship-loc s))
  (place-image (render-ship s)
               (posn-x loc) (posn-y loc)
               scene))

(define (render-ship s)
  (rotate (ship-angle s) SHIP-IMG))

(define (new-ship)
  (ship (posn (/ WIDTH-PX 2) (/ HEIGHT-PX 2))
        (posn 0 0)
        (posn 0 0)
        0
        #f))
(define (new-ship2)
  (struct-copy ship (new-ship) [accel (posn 0 -1)]))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (posn+ p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))))
(module+ test
  (check-equal? (posn+ (posn 1 2) (posn 3 4)) (posn 4 6)))

(define (posn-rot p angle)
  (define cos-angle (cos (degrees->radians angle)))
  (define sin-angle (sin (degrees->radians angle)))
  (posn (+ (* cos-angle (posn-x p))
           (* sin-angle (posn-y p)))
        (+ (* -1 sin-angle (posn-x p))
           (* cos-angle (posn-y p)))))

(module+ test
  (check-equal? (posn-rot (posn 1 0) 0) (posn 1 0)))

(define (pos-equation x v a)
  (+ x (* v TICK-RATE) (* 1/2 a (sqr TICK-RATE))))
(define (vel-equation v a)
  (+ v (* a TICK-RATE)))
(module+ test
  (define test-vel (/ 1 TICK-RATE))
  (define test-accel (sqr test-vel))
  (check-equal? (pos-equation 0 test-vel test-accel) 3/2)
  (check-equal? (vel-equation 10 test-accel) (+ 10 test-vel)))

(define (next-loc loc vel accel)
  (posn (pos-equation (posn-x loc) (posn-x vel) (posn-x accel))
        (pos-equation (posn-y loc) (posn-y vel) (posn-y accel))))
(define (next-vel vel accel)
  (posn (vel-equation (posn-x vel) (posn-x accel))
        (vel-equation (posn-y vel) (posn-y accel))))

(define (maybe-loop-bounds p)
  (define (coord-adjust c maxbound)
    (cond [(< c 0) (+ c maxbound)]
          [(> c maxbound) (- c maxbound)]
          [else c]))
  (define (coord-adjust-width c) (coord-adjust c WIDTH-PX))
  (define (coord-adjust-height c) (coord-adjust c HEIGHT-PX))
  (lens-transform posn-y-lens
                  (lens-transform posn-x-lens p coord-adjust-width)
                  coord-adjust-height))

(module+ test
  (check-equal? (maybe-loop-bounds (posn -1 (add1 HEIGHT-PX))) (posn (sub1 WIDTH-PX) 1))
  (check-equal? (maybe-loop-bounds (posn 1 2)) (posn 1 2)))

(define (next-ship s)
  (struct-copy ship s
               [loc (maybe-loop-bounds (next-loc (ship-loc s) (ship-vel s) (ship-accel s)))]
               [vel (next-vel (ship-vel s) (ship-accel s))]
               [accel (if (ship-engine-on s)
                          (posn-rot (posn 0 (- SHIP-ACCEL-RATE)) (ship-angle s))
                          (posn 0 0))]))
(module+ test
  (check-equal? (next-ship (new-ship)) (new-ship)))
