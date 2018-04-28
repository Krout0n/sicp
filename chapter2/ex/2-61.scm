(load "../../lib.scm")

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(trace adjoin-set)

(adjoin-set 5 (list 1 2 3 6 7 8))

