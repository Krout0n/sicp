; EX2.17
(define (last-pair items)
    (if (= (length items) 1)
        (car items)
    (last-pair (cdr items))))

; EX2.18
(define (append f s)
    (if (null? f)
        s
    (cons (car f) (append (cdr f) s))))

(define (reverse items)
    (if (null? (cdr items))
        items
    (append (reverse (cdr items)) (list (car items))))) ; むずい

; EX2.19 知らん

; EX2.20
(define (same-parity . items)
    (define parity (modulo (car items) 2))
    (define (same-parity-iter parity items result)
        (if (null? items)
            result
        (if (= parity (modulo (car items) 2))
            (same-parity-iter parity (cdr items) (append result (list (car items))))
            (same-parity-iter parity (cdr items) result))))
    (same-parity-iter parity  items ()))
