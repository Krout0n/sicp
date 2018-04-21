(define (for-each proc l) ; if式だとelseが死ぬ
  (define (iter r)
    (cond ((null? r) #t)
          (else (proc (car r))
                (iter (cdr r)))))
  (iter l))

(for-each (lambda (x) (newline) (print x)) (list 1 2 3 4))

