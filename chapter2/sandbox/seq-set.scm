(define (element-of-set? x set) ; setはsortedである前提
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(load "../../lib.scm")
(element-of-set? 3 (list 1 2 4 5))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2)) (list)
    (let ((x1 (car s1))
          (x2 (car s2)))
      (cond
        ((= x1 x2) (cons x1 (intersection-set (cdr s1) (cdr s2))))
        ((< x1 x2) (intersection-set (cdr s1) s2))
        (else (intersection-set s1 (cdr s2)))))))

(trace intersection-set)
(intersection-set (list 2 3 5 7) (list 1 2 3 4))

