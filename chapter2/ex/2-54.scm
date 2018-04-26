(define (enumerate-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (equal? _l _r)
  (define (iter l r)
    (cond ((and (null? l) (null? r)) #t)
          ((not (eq? (car l) (car r))) #f)
          (else (iter (cdr l) (cdr r)))))
  (iter (enumerate-tree _l) (enumerate-tree _r)))

(print (equal? '(this is a list) '(this (is a) list)))

