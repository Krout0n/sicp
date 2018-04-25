(load "../../lib.scm")

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
      (enumerate-intervals 1 (- i 1))))
    (enumerate-intervals 1 n)))


(define (prime-sum-pairs n)

  (define (prime-sum-pairs? pair)
    (prime? (+ (car pair) (cadr pair))))

  (define (make-sum-pair pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

  (let ((uniq-pairs (unique-pairs n)))
        (map make-sum-pair (filter prime-sum-pairs? uniq-pairs))))

(print (prime-sum-pairs 10)) ; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11) (7 4 11) (7 6 13) (8 3 11) (8 5 13) (9 2 11) (9 4 13) (9 8 17) (10 1 11) (10 3 13) (10 7 17) (10 9 19))

