(define nil '())
(define (subsets s)
  (if (null? s) (list (list)) ; (list (list)) じゃないとだめ
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(use slib)
(require 'trace)
(trace subsets)
(define tester (list 1 2 3))
(print (subsets tester)) ; ( () (3) (2) (2 3) 1 (1 2) (1 3) (1 2 3))
