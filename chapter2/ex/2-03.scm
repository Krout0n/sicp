(define (x-point p)(car p))
(define (y-point p)(cdr p))

(define (length p1 p2)
    (let ((p1x (x-point p1))
          (p2x (x-point p2))
          (p1y (y-point p1))
          (p2y (y-point p2)))
      (let ((x (- p2x p1x))
            (y (- p2y p1y)))
        (sqrt (+ (* x x)(* y y))))))

(define (make-rectangle p1 p2 p3 p4)(list p1 p2 p3 p4))

(define (one rentagle)
  (let ((p1 (car rentagle))
        (p2 (car (cdr rentagle))))
    (length p1 p2)))

(define (another rentagle)
  (let ((p2 (car (cdr rentagle)))
        (p3 (car (cdr (cdr rentagle)))))
    (length p2 p3)))

(define (circum rentagle)
  (* 2 (+ (one rentagle) (another rentagle))))

(define (surface rentagle)
  (* (one rentagle) (another rentagle)))

(define ren (make-rectangle (cons 0 1)(cons 0 0)(cons 1 0)(cons 1 1)))
(print (circum ren)) ; 4
(print (surface ren)) ; 1

