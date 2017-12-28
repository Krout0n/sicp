(use slib)
(require 'trace)

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(trace accumulate)

; EX2.33
(define (map proc sequence)
    (accumulate (lambda (x y) (cons (proc x) y)) (list) sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(display (length (list 1 2 3 4 5)))

(define (horner-eval x sequence)
    (accumulate (lambda (this-coeff higher-terms)()) 0 sequence))
; (display (horner-eval 2 (list 1 3 0 5 0 1)))