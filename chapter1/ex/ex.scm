(define (square x)(* x x))

(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (expt b n)
    (if (= n 0) 1
    (* b (expt b (- n 1)))))


(define (iterative-expt b n)
    (if (= n 0) 1
        (* b (iterative-expt b (- n 1)))))

; ex1.16
(define (iterative-fast-expt a b n)
    )

; ex1.17
(define (multi a b)
    (define (double x)(+ x x))
    (define (halve x)(/ x 2))
    (cond ((< a 0) (* -1 (multi (* -1 a) b)))
          ((< b 0) (* -1 (multi a (* -1 b))))
          ((= b 0) 0)
          ((even? b) (multi (double a) (halve b)))
          (else (+ a (multi a (- b 1))))))
