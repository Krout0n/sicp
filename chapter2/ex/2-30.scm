(define (square-tree tree)
  (map (lambda (t)
         (if (not (pair? t)) (square t)
           (square-tree t)))
       tree))

(define tester (list 1(list 2 (list 3 4) 5) (list 6 7)))
(print tester)
(use slib)
(require 'trace)
(trace square-tree)
(print (square-tree tester))

(define (square-tree tree)
  (cond ((null? tree) (list))
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

(trace square-tree)
(print (square-tree tester))

