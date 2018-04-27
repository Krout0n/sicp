(define (element-of-set? x set)
  (cond ((null? x) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set x (cdr x)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
    (cons x set)))

