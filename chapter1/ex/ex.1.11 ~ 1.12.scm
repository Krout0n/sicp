; 1.11

(define (fact n)
  (define (fact-iter product k)
    (if (= k 1)
     product
      (fact-iter (* product k) (- k 1))))
  (fact-iter 1 n))

(define (f n)
  (if (< n 3) n
    (+
      (f (- n 1))
      (* 2 (f (- n 2)))
      (* 3 (f (- n 3))))))

(define (f n)
  (define (f-iter k product))
  (f-iter k 0)
)

; 1.12
(define (pascal n k)
  (if
    (or
      (= n 0)
      (= k n)) 1
    (+
      (pascal n (- k 1))
      (pascal (- n 1) (- k 1)))))

(define (fib n)
  (cond
    ((= n 0) 1)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))


; 1 1 1 1 1
; 1 2 3 4
; 1 3 4
; 1 4