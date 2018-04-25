(use slib)
(require 'trace)

(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (prime? n)

  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (divides? n d)
    (= (modulo n d) 0))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (= n (smallest-divisor n)))

(define (enumerate-trees tree)
  (cond ((null? tree) (list)) 
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-trees (car tree))
                      (enumerate-trees (cdr tree))))))

(define (enumerate-intervals low high)
  (if (< high low) (list)
    (cons low (enumerate-intervals (+ 1 low) high))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

