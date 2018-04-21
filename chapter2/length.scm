(define (x-point p)(car p))
(define (y-point p)(cdr p))

(define (length p1 p2)
    (let ((p1x (x-point p1))
          (p2x (x-point p2))
          (p1y (y-point p1))
          (p2y (y-point p2)))
      (let ((x (- p2x p1x))
            (y (- p2y p1y)))
        (sqrt (+ (* x x)(+ y y))))))