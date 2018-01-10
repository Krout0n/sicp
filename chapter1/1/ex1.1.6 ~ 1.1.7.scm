(define (square x)(* x x))

(define (abs x)
    (if (< x 0) (- x) x))

(define (average x y)(/ (+ x y) 2))

(define enough (/ 1.0 100000000000000000000000000))

(define (good-enough? guess before-guess)
    (< (abs (- before-guess guess)) enough))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess before-guess x)
    (print guess " " before-guess)
    (if (good-enough? guess before-guess)
        guess (sqrt-iter (improve guess x) guess x)))

(define (sqrt x) (sqrt-iter 1.0 1145141919810364364 x))

; (sqrt (square(/ 1.0 1145141919810364364)))
(define (cbrt-improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cbrt-iter guess before-guess x)
    (print guess " " before-guess)
    (if (good-enough? guess before-guess)
        guess (cbrt-iter (cbrt-improve guess x) guess x)))

(define (cbrt x) (cbrt-iter 1.0 1145141919810364364 x))