(load "../../lib.scm")

(define (uniq-ijk n)
  (flatmap
    (lambda (i)
      (flatmap
        (lambda (j)
          (map (lambda (k) (list k j i))
               (enumerate-intervals 1 (- j 1))))
        (enumerate-intervals 1 (- i 1))))
    (enumerate-intervals 1 n)))

(define (is-sum-s? s ijk)
   (= s (+ (car ijk) (cadr ijk) (caddr ijk))))

(define (find-s n s)
  (let ((ijk (uniq-ijk n)))
    (filter (lambda (t) (is-sum-s? s t)) ijk)))

(print (find-s 10 11)) ; ((2 4 5) (2 3 6) (1 4 6) (1 3 7) (1 2 8))
