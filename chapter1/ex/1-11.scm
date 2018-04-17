(define (f n)
  (if (< n 3) n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(print (f 10)) ; 1892

(define (f n)
  (define (iter a b c m)
    (cond ((< n 3) n)
          ((= m (+ n 1)) c)
          (else (iter b c (+ (* 3 a) (* 2 b) c) (+ m 1)))))
  (iter 0 1 2 3))

(print (f 10)) ; 1892


