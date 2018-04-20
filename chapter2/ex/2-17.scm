(define (length l)
  (if (pair? l) (+ 1 (length (cdr l)))
    0))

(define (last-pair l)
    (define len (- (length l) 1))
    (define (iter c r)
      (if (= len c) (car r)
      (iter (+ 1 c)(cdr r))))
    (iter 0 l))

(print (last-pair(list 1 2 3 4 5))) ; 5
(print (last-pair (list 5 4 3 2 1))) ; 1
