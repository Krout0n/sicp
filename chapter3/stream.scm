(define the-empty-stream '())

(define (stream-null? s) (null? s))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define (stream-ref n s)
  (if (= n 0) (stream-car s)
    (stream-ref (- n 1) (stream-cdr s))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each print s))

