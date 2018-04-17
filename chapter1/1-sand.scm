(define (good? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (improve guess x)
  (define (average x y)(/ (+ x y) 2))
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good? guess x) guess
    (sqrt-iter (improve guess x) x)))

(define (s x)(sqrt-iter 1.0 x))
(use slib)
(require 'trace)
(trace sqrt-iter)
(print (sqrt-iter 1.0 2))
