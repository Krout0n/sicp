(define (append l1 l2)
  (if (null? l1) l2
    (cons (car l1) (append (cdr l1) l2))))

(print (append (list 1 2)(list 3 4)))
(define x (list (list 1 2) (list 3 4)))

; (fringe x) -> (list 1 2 3 4)

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(use slib)
(require 'trace)
(trace fringe)
(trace append)
(print (fringe x))
