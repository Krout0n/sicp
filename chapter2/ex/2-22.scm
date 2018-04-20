(define (square-list items)
  (define (iter l1 l2)
    (if (null? l1) l2
    (iter (cdr l1) (cons (* (car l1)(car l1)) l2))))
  (iter items (list)))

(print (square-list (list 1 2 3 4 5)))
