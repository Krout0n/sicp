(define tester (list 1 2 3 4 5))

(define (reverse l)
  (define (iter l1 l2)
    (if (null? l1) l2
      (iter (cdr l1) (cons (car l1) l2))))
  (iter l (list)))

(print (reverse tester)) ; (5 4 3 2 1)
