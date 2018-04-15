(define (gcd m n)
    (if (= (modulo m n) 0) n 
      (gcd n (modulo m n))))

(define (abs x)
    (if (> x 0) x
      (* -1 x)))

(define (make-rat p q)
    (let ((g (gcd (abs p)(abs q))))
      (if (< (* p q) 0) (cons (/ (* -1 (abs p)) g)(/ (abs q) g))
      (cons (/ (abs p) g) (/ (abs q) g)))))

(print (make-rat 35 21)) ; (5 . 3)
(print (make-rat -35 21)) ; (-5 . 3)
(print (make-rat -35 -21)) ; (5. 3)
(print (make-rat 35 -21)) ; (-5. 3)
