(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)(cons x set))
(define (union-set s1 s2)(append s1 s2))
(define (intersection-set s1 s2)
  (cond ((null? s1) (list))
        ((element-of-set? (car s1) s2) (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

; 優れている場合 ・・・　何も思い浮かばない, 入力されるデータに偏りがある場合など？

