(load "../../lib.scm")

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(trace make-cycle)
(define z (make-cycle (list 'a 'b 'c)))

(print z)
(last-pair z)

