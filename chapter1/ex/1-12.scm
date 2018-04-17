(define (pascal n k)
  (if (or (= n 0) (= k 0) (= n k)) 1
    (+ (pascal (- n 1) k) (pascal (- n 1) (- k 1)))))

(print (pascal 0 0)) ; 1
(print (pascal 1 0)) ; 1
(print (pascal 2 1)) ; 2
(print (pascal 3 1)) ; 2
(print (pascal 4 2)) ; 6
