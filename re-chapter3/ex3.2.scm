(define (make-monitored f)
    (define mf 0)
    (lambda (arg)
        (cond
            ((eq? arg 'how-many-calls?) mf)
            (else
                (begin
                    (set! mf (+ mf 1))
                    (f arg))))))

(define s (make-monitored sqrt))
(define d (make-monitored (lambda (x) (* x x))))
(define (test obj val expected)
    (display (if (= (obj val) expected) "" "満足できねえぜ")))

(test s 100 10)
(test s 'how-many-calls? 1)
(test s 100 10)
(test s 'how-many-calls? 2)

(test d 100 10000)
(test d 'how-many-calls? 1)
(test d 1 1)
(test d 'how-many-calls? 2)
