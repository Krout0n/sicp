(define (my-cons x y)
    (lambda (n)
      (cond ((= n 0) x)
            ((= n 1) y)
            (else (error "err")))))

(define (my-car n)(n 0))
(define (my-cdr n)(n 1))
(define m (my-cons 1 (my-cons 2 3)))
(print (my-car m)) ; 1
(print (my-cdr(my-cdr m))) ; 3

