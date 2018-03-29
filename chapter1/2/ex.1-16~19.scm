; ex-1.16
(define (fast-expt b n)
    (define (fast-e a b n)
        (cond
            ((= n 0) a)
            ((even? n) (fast-e a (square b) (/ n 2)))
            (else (fast-e (* a b) b (- n 1)))))
    (fast-e 1 b n))

; ex-1.17
(define (fast-multiply x y)
(define (double x)(+ x x))
(define (halve x)(/ x 2))
(cond
    ((= y 0) 0)
    ((= y 1) x)
    ((even? y) (fast-multiply (double x) (halve y)))
    (else (+ x (fast-multiply x (- y 1))))))

; (lambda (x) (fast-multiply 12 x))  (iota 11 0))
; (0 12 24 36 48 60 72 84 96 108 120)

; ex-1.18
; ex-1.19