(define (print-tree-diagram p1 p2 str)
  (cond ((and (null? p1) (null? p2)) (print str))
        ((null? p1) (print-tree-diagram p1 (cdr p2) (string-append str (car p2))))
        ((null? p2) (print-tree-diagram (cdr p1) p2 (string-append str (car p1))))
        (else (print-tree-diagram (cdr p1) p2 (string-append str (car p1)))
              (print-tree-diagram p1 (cdr p2) (string-append str (car p2))))))

(print-tree-diagram (list "a" "b" "c" "d") (list "e" "f" "g" "h" "i") "")

