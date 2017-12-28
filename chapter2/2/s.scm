(define (square x)(* x x))

(define (sum-odd-squreas tree)
    (cond
        ((null? tree) 0)
        ((not (pair? tree))
            (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squreas (car tree)) (sum-odd-squreas (cdr tree))))
    )
)

; (display (sum-odd-squreas (list 1 2 3 4 5 6)))

(define (filter predicate sequence)
    (cond 
        ((null? sequence) '())
        ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))
    )
)

; (display(filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))
    )
)
