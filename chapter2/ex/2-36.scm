(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq) ; (lambda (x y))  x <- 今回の要素, y <- 俺たちが今まで積み上げてきたもん
        (accumulate op init (cdr seq)))))

(use slib)
(require 'trace)
(trace accumulate)


(define (accumulate-n op init seqs)
  (if (null? (car seqs)) (list)
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9))))

