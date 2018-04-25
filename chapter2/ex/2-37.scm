(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) (list)
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(use slib)
(require 'trace)

(define m (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define v (list 1 0 0 1))

(define (matrix-*-vector m v)
  (map (lambda (_m) (accumulate + 0 (map * _m v))) m))

; my code was equal to (dot-product...)
(define (matrix-*-vector m v)
  (map (lambda (_m) (dot-product _m v)) m))

(print (matrix-*-vector m v))

(define (transpose m)
  (accumulate-n cons (list) m))

(print (transpose m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (_m) (matrix-*-vector cols _m)) m)))

(print(matrix-*-matrix m (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))))

