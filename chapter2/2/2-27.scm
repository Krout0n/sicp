(define (reverse l)
  (define (iter l1 l2)
    (cond ((null? l1) l2)
          ((not (pair? l1)) l1)
          (else (iter (cdr l1) (cons (car l1) l2)))))
  (iter l (list)))

(define (deep-reverse l)
  (reverse (map reverse l)))

(define test-set (list (list 1 2) 3 (list 3 4)))
(print (deep-reverse test-set)) ; ((4 3) 3 (2 1))

