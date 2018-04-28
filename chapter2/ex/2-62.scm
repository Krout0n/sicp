(load "../../lib.scm")

(define (union-set l r)
  (define (iter s1 s2 result)
    (if (and (null? s1) (null? s2)) result
      (let ((x1 (if (null? s1) (least-fixnum) (car s1)))
            (x2 (if (null? s2) (least-fixnum) (car s2))))
        (cond ((< x1 x2) (iter s1 (cdr s2) (cons x2 result)))
              ((= x1 x2) (iter (cdr s1) (cdr s2) (cons x1 result)))
              (else (iter (cdr s1) s2 (cons x1 result)))))))
  (iter (reverse l) (reverse r) (list)))

(trace union-set)
(union-set (list 1 2 5 9) (list 3 4 7 8))
; CALL union-set (1 2 5 9) (3 4 7 8)
; RETN union-set (1 2 3 4 5 7 8 9)
(union-set (list 2 4 6 8) (list 2 3 5 7))
; CALL union-set (2 4 6 8) (2 3 5 7)
; RETN union-set (2 3 4 5 6 7 8)

