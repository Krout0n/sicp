(define g
    (let ((x '()) (not-assined? #t))
    (lambda (v)
        (if not-assined?
            (begin (set! x v) (set! not-assined? #f) v)
            x))))