; 集合を表現する方法として重複なしのリストを構成する方法がある．
; element-of-set? はmemqと似たような実装になる.

(load "../lib.scm")

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 (list 5 6 1 8 2))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
    (cons x set)))

(adjoin-set 1 (list 2 3 4 5))

(define (intersection-set s1 s2) ; 積集合
  (cond ((or (null? s1) (null? s2)) (list))
        ((element-of-set? (car s1) s2) (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) (cdr s2))))) 

(trace intersection-set)
(intersection-set (list 2 3 5 7) (list 2 4 6 8))

