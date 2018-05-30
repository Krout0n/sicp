(define (myap proc l)
  (if (null? l) (list)
    (cons (proc (car l)) (myap proc (cdr l)))))

(print (myap (lambda (x)(* x x)) (list 1 2 3 4 5)))
