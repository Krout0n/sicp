(define (sum term a next b)
    (define (iter a product)
        (if (> a b) product
          (iter (next a) (+ (term a) product))))
    (iter a 0))

(define (cube x)(* x x x))
(define (inc x) (+ x 1))

(print (sum cube 1 inc 10)) ; 3025
