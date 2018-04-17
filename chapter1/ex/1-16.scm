(define (fast-expt b n)
  (define (iter a b n)
    (cond
      ((= n 0) a)
      ((= (modulo n 2) 0) (iter a (* b b) (/ n 2)))
      (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(map (lambda (x)(print (fast-expt 2 x))) (list 1 2 3 4 5 6 7 8 9 10))
