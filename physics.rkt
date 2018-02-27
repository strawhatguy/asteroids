(module physics racket
  (provide (all-defined-out))
  
  (define (pos-equation x v a dt)
    (+ x (* v dt) (* 1/2 a (sqr dt))))
  (define (vel-equation v a dt)
    (+ v (* a dt)))
  
  (module* test #f
    (require rackunit)
    (define TICK-RATE 10)
    (define test-vel (/ 1 TICK-RATE))
    (define test-accel (sqr test-vel))
    (check-equal? (pos-equation 0 test-vel test-accel TICK-RATE) 3/2)
    (check-equal? (vel-equation 10 test-accel TICK-RATE) (+ 10 test-vel)))
  )