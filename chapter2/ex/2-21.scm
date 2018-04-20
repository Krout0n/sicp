(define (square-list items)
  (if (null? items) (list)
    (cons (* (car items)(car items)) (square-list (cdr items)))))


(print (square-list (list 1 2 3 4 6)))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(print (square-list (list 1 2 3 4 6)))
