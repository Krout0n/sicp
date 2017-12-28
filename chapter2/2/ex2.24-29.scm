; ex2.25
(car(cdr(car(cdr(cdr (list 1 3 (list 5 7) 9)))))) ; 7
(car (car (list (list 7)) )) ; 7
(define x (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr (car (cdr x)))))))))))) ; 7

; ex2.26
(define (length items)
    (define (length-iter a count)
        (if (null? a) count)
            (length-iter (cdr a) (+ count 1)))
    (length-iter items 0))

(define (append f s)
    (if (null? f) s
        (cons (car f) (append (cdr f) s))))

; (define x (list 1 2 3))
; (define y (list 4 5 6))
; (display "(append x y)->")
; (display (append x y))
; (newline)
; (display "(cons x y)->")
; (display (cons x y))
; (newline)
; (display "(list x y)->")
; (display (list x y))
; (newline)

; ex2.27
(define x (list (list 1 2) (list 3 4)))
(define (reverse items)
    (if (null? (cdr items))
        items
    (append (reverse (cdr items)) (list (car items))))) ; むずい

(define (map proc items)
    (if (null? items) '()
        (cons (proc (car items)) (map proc (cdr items)))))

(reverse x) ; ((3 4) (1 2))
(define (deep-reverse l)
    (define x (reverse l))
    (map reverse x))

(deep-reverse x); ((4 3) (2 1))

; ex.2.28
(use slib)
(require 'trace)
(define x (list (list 1 2) (list 3 4)))
(define (fringe x)
    (cond
        ((null? x) 0)
        ((pair? x) (car x))
        (else (+ (fringe (car x))
                (fringe (cdr x))))))
(trace fringe)
(display(fringe x)) ; (1 2 3 4)
(newline)
(display(fringe (list x x))) ; (( (1 2) (3 4)) ((1 2) (3 4))) -> (1 2 3 4 1 2 3 4)
