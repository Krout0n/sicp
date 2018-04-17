(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate combiner null-value term (next a) next b)
        )
    )
)

(define (sum a b)
    (define (inc n)(+ n 1))
    (accumulate + 0 + 1 inc 10)
)

(define (product a b)
    (define (inc n)(+ n 1))
    (accumulate * 1 * 1 inc 10)
)

(define (square x) (* x x))

(define (divides? a b)
 (= (remainder b a) 0))

(define (find-divisor n test-divisor)
 (cond ((> (square test-divisor) n) n)
       ((divides? test-divisor n) test-divisor)
       (else (find-divisor n (+ test-divisor 1)))))

(define (smalltest-divisor n)
 (find-divisor n 2))

(define (prime? n)
 (= n (smalltest-divisor n)))

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate combiner null-value term (next a) next b)
        )
    )
)

(define (filtered-accumulate filter combiner null-value term a next b)
    (cond
        ((> a b) null-value)
        ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
        ; (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))
    )
)

(define (sum-prime a b)
    (define (inc n)(+ n 1))
    (define (square x)(* x x))
    ; (- (filtered-accumulate prime? + 0 square 1 inc 10) 1)
    (filtered-accumulate prime? + 0 square a inc b))
