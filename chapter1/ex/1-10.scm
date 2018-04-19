(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; (print (A 1 10))
; (print (A 2 4))
; (print (A 3 3))

(define (f n) (A 0 n)) ; f(n) = 2*n
(print (f 10)) ; 20

(define (g n) (A 1 n)) ; g(n) = 2^n
(print (g 2)) ; 4
(print (g 10)) ; 1024

(define (h n) (A 2 n)) ; h(n) = 2^h(n-1) = 2 ^ (2 ^ g(n-1)) = 2 ^ (2 ^ (2 ^ (n - 1)))

(print (h 0)) ; 0
(print (h 1)) ; 2
(print (h 2)) ; 4 = (2) ^ 2
(print (h 3)) ; 16 = (2 ^ 2) ^ 2
(print (h 4)) ; 65536 = ((((2^2)^2)^2)^2)
(print (h 5)) ; Large Numbers

