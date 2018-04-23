(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (my-map p seq)
  (accumulate (lambda (x y)(cons (p x) y)) (list) seq))

(define (my-append l r)
  (accumulate cons r l))

(define (my-length l)
  (accumulate (lambda (x y) (+ 1 y)) 0 l))

(use slib)
(require 'trace)
(trace accumulate)

(print (my-map (lambda (x) (* x 2)) (list 1 2 3)))
(print (my-append (list 1 2 3) (list 4 5 6)))
(print (my-length (list 1 2 3 5)))
