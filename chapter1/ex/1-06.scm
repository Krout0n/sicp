(define (new-if pre then els)
  (cond (pre then)
        (else els)))

(define (good? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (improve guess x)
  (define (average x y)(/ (+ x y) 2))
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (new-if (good? guess x) guess
    (sqrt-iter (improve guess x) x))) ; new-ifはただの手続き && Schemeは適用順序評価であるので先に(sqrt-iter ..)が処理される.

(use slib)
(require 'trace)
(trace sqrt-iter)
(print (sqrt-iter 1.0 2.0))

; CALL sqrt-iter 1.0 2.0
;   CALL sqrt-iter 1.5 2.0
;     CALL sqrt-iter 1.4166666666666665 2.0
;       CALL sqrt-iter 1.4142156862745097 2.0
;         CALL sqrt-iter 1.4142135623746899 2.0

