; xのcount-leavesは(car x)のcount-leaves と (cdr x)のcount-leavesを足しあわせたものである．
(define (count-leaves tree)
  (cond ((null? tree) 0)
         ((not (pair? tree)) 1)
         (else (+ (count-leaves (car tree))
                  (count-leaves (cdr tree))))))

; (print (count-leaves (list (list 1 2 (list 3 4)) (list (list 1 2) 3 4))))
(print (count-leaves (list 1 2 3 4)))
(print (count-leaves (list 1 2 (list 3 4 5) (list 6 7 8) 9 10)))
