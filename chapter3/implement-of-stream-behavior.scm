(load "./stream.scm")
(load "../lib.scm") ; prime?

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (cons-stream (stream-car s)
                                            (stream-filter pred
                                                           (stream-cdr s))))
        (else (stream-filter pred
                             (stream-cdr s)))))

(define (stream-enumerate-intervals a b)
  (if (> a b) the-empty-stream
    (cons-stream a (stream-enumerate-intervals (+ a 1) b))))

 (print
   (stream-car
     (stream-cdr
       (stream-filter prime?
                      (stream-enumerate-intervals 10000 10000000)))))

