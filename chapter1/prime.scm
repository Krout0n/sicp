(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? n d)
  (= (modulo n d) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(print (prime? 3))

