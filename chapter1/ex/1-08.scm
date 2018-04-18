(define (improve x y)
  (/ (+ (/ x (* y y)) (* 2.0 y)) 3.0))

(define (good? ex-y y)
  (< (abs (- ex-y y)) 0.00001))

(define (cube-root-iter ex-y y x)
  (if (good? ex-y y) y
    (cube-root-iter y (improve x y) x)))
  
(define (curt x)
  (cube-root-iter 1 x x))
  
(print (curt 27)) ; 3.0

