(define (reverse seq)
  (fold-right (lambda (x y) (append y (list x))) (list) seq))

(print (reverse (list 4 3 2 1))) ; (1 2 3 4)

(define (reverse seq)
  (fold-left (lambda (x y)(cons y x)) (list) seq))

(print (reverse (list 4 3 2 1))) ; (1 2 3 4)

