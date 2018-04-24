(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq) ; (lambda (x y))  x <- 今回の要素, y <- 俺たちが今まで積み上げてきたもん
        (accumulate op init (cdr seq)))))

(use slib)
(require 'trace)
; (trace accumulate)

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves tree)
  (accumulate (lambda (x y) (+ x y)) 0 (map (lambda (x) (length (enumerate-tree x))) tree)))

(define tester (list 1 2 (list 3 4) (list 5 6 (list 7 8) 9) 10))
; (print tester)
; (print (map (lambda (x) (length (enumerate-tree x))) tester))
; (print (enumerate-tree tester))
; (print (count-leaves tester))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) tree)))

(trace count-leaves)
(print (count-leaves tester))
; CALL count-leaves (1 2 (3 4) (5 6 (...) 9) 10)
;   CALL count-leaves (3 4)
;   RETN count-leaves 2
;   CALL count-leaves (5 6 (7 8) 9)
;     CALL count-leaves (7 8)
;     RETN count-leaves 2
;   RETN count-leaves 5
; RETN count-leaves 10
; 10
