(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x) ; (b)

(define (append! x y)
  (set-cdr! y (last-pair x)) x)

(define (n x y)
  (square x)
  (sqrt y))

(define w (append! x y))
(print y)
(print (cdr x))
