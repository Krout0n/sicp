(define (sum term a next b)
    (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(print (sum (lambda (x)(* x x x)) 1 (lambda (x) (+ x 1)) 10))

