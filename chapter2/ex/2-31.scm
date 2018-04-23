(define tester (list 1(list 2 (list 3 4) 5) (list 6 7)))
(use slib)
(require 'trace)

(define (tree-map proc tree)
  (cond ((null? tree) (list))
        ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        (else (proc tree))))

(trace tree-map)
(print (tree-map (lambda (x) (* x 10)) tester))

