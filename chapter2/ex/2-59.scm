(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
    (cons x set)))

(define (union-set s1 s2) ; 和集合
  (define (iter l r)
    (if (null? l) r
      (iter (cdr l) (adjoin-set (car l) r))))
  (iter s1 s2))

(load "../../lib.scm")

(trace union-set)
(trace adjoin-set)
(define odd (list 1 3 5 7))
(define even (list 2 4 6 8))
(define prime (list 2 3 5))

(union-set odd even)
(union-set prime odd)
(union-set even prime)
