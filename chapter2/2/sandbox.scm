(define one-through-four (list 1 2 3 4))
(car one-through-four) ; 1
(cdr one-through-four) ; (2 3 4)
(car (cdr one-through-four)) ; 2
(cons 10 one-through-four) ; (10 1 2 3 4)
; ...

(define (list-ref items n) ; リストであるitemsとnを受け取って該当する要素を返す．
    (if (= n 0)
        (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 4) ; 25

(define (length items)
    (if (null? items) 0)
        (+ 1 (length (cdr items))))

(define (length items)
    (define (length-iter a count)
        (if (null? a) count)
            (length-iter (cdr a) (+ count 1)))
    (length-iter items 0))

(define odds (list 1 3 5 7))

; (append odds squares) をした場合 (1 3 5 7 1 4 9 16 25)
; (append squres odds) をした場合 (1 4 9 16 25 1 3 5 7)なappendを考える．

(define (append f s)
    (if (null? f) s
    (cons (car f) (append (cdr f) s))))
    ; (append (cdr f) (cons (car f) s)))

(define (map proc items)
    (if (null? items) '()
        (cons (proc (car items)) (map proc (cdr items)))))

; (display (map abs (list -10 2.5 -11.6 17)))
; (display (map (lambda (x) (* x x)) (list 1 2 3 4)))
(define (square-list items)
    (if (null? items)
        '()
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
    (map (lambda (x) (* x x)) items))
; (display (square-list (list 1 2 3 4)))

; (define (for-each proc items)
;     (define (each-iter proc items)
;         (if (null? items) '()
;             (proc (car items))
;             (each-iter proc (cdr items))))
;     (each-iter proc items))

; (for-each (lambda (x) (newline)(display x)) (list 57 321 88))
(define x (cons (list 1 2) (list 3 4)))
(define (count-leaves x)
    (cond
        ((null? x) 0)
        ((not(pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (scale-tree tree factor)
    (cond
        ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))

(use slib)
(require 'trace)
(trace scale-tree)
; (display (scale-tree (list 1 (list 2) (list 3 4)) 10))

(define (sum-odd-squares tree)
    (cond
        ((null? tree) 0)
        ((not (pair? tree))
            (if (odd? tree) (square tree)0))
        (else (+ (sum-odd-squares (car tree)) (sum-odd-squares (cdr tree))))
    ))
(define (square x) (* x x))
(trace sum-odd-squares)
; (display (sum-odd-squares (list 1 (list 2) (list 3 4 5 6 7 (list 8)))))

(define (filter predicate sequence)
    (cond
        ((null? sequence) '())
        ((predicate (car sequence))
            (cons
                (car sequence)
                (filter predicate (cdr sequence))
        ))
        (else
            (filter predicate (cdr sequence)))
))

(trace filter)
; (display (filter odd? (list 1 2 3 4 5)))

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(trace accumulate)
; (display (accumulate + 0 (list 1 2 3 4 5)))
; (display (accumulate * 1 (list 1 2 3 4 5)))
; (display (accumulate cons (list) (list 1 2 3 4 5)))

(define (enumerate-interval low high)
    (if (> low high)
        '()
    (cons low (enumerate-interval (+ low 1) high))
))
(trace enumerate-interval)
; (display (enumerate-interval 1 10))

(define (enumerate-tree tree)
    (cond
        ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
            (append
                (enumerate-tree (car tree))
                (enumerate-tree (cdr tree))
            ))
))
; (display (enumerate-tree (list 1 (list 2 (list 3 4) 5))))

; sum-odd-squaresはこう書くことができる
; 引数の木を平らにして，odd?のfilterにかける
; それらをsquareして, accumulateで合計だして，終わり！

(define (sum-odd-squares tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

; (display (sum-odd-squares (list 1 2 3 (list 4 5) (list 6 7(list (list 8))))))

(define (fib n)
    (cond
        ((= n 0) 0)
        ((= n 1) 1)
    (else
        (+ (fib (- n 1)) (fib (- n 2))))
))

(define (even-fibs n)
    (accumulate + 0 (map fib (filter even? (enumerate-interval 0 n)))))

; (display (even-fibs 10))

; (display (fib 10))

(define (list-fib-squares n)
    (accumulate cons (list)
        (map square (map fib (enumerate-interval 0 n)))
))

; (display (list-fib-squares 10))

