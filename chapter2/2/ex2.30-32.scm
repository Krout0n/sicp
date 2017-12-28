; EX2.30
(use slib)
(require 'trace)
(define (square-tree tree)
    (cond ((null? tree) '())
    ((not (pair? tree)) (* tree tree))
    (else
        (cons (scale-tree (car tree)) (scale-tree (cdr tree))))))
; (trace square-tree)
; (display (square-tree (list 1 (list 2) (list 3 4))))

; EX2.31
(define (tree-map proc tree)
    (cond
        ((null? tree) '())
        ((not(pair? tree)) (proc tree))
        (else
            (cons (tree-map proc (car tree))
                  (tree-map proc (cdr tree))))))

(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree))
(display (square-tree (list 1 (list 2) (list 3 4))))
(display (square-tree (list 1 (list 2) (list 3 4)(list 5 6)(list 7 (list 8 9)))))