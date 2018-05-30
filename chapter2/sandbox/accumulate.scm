(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq) ; (lambda (x y))  x <- 今回の要素, y <- 俺たちが今まで積み上げてきたもん
        (accumulate op init (cdr seq)))))

(use slib)
(require 'trace)
(trace accumulate)
(print (accumulate + 0 (list 1 2 3 4 )))
(print (accumulate * 1 (list 2 3 4 5)))
(print (accumulate cons (list) (list 1 2 3 4 5)))
