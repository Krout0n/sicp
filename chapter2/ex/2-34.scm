(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq) ; op -> l(lambda (x y)) x -> (car seq), y -> initを数回opしたもの
        (accumulate op init (cdr seq)))))

(define (honor x seq)
  (accumulate (lambda (this-coe higher)
                (+ (* higher x) this-coe))
              0 seq))

(use slib)
(require 'trace)
(trace accumulate)
(print (honor 2 (list 1 3 0 5 0 1)))

