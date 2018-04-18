(define (good? guess ex-guess)
  (< (abs (- guess ex-guess)) 0.001))

(define (improve guess x)
  (define (average x y)(/ (+ x y) 2))
  (average guess (/ x guess)))

(define (sqrt-iter guess ex-guess x)
  (if (good? guess ex-guess) guess
    (sqrt-iter (improve guess x) guess x)))

(define (s x)(sqrt-iter 1.0 x x))
(use slib)
(require 'trace)
(trace sqrt-iter)
(print (s 10000000000000000000000000000000000000000000000000000000000000000000000000000)) ; 1.0e38
(print (s 0.00001)) ; 0.003172028655357483
(print (s 0.00000001)) ; 9.799734463768973e-4

