(define f
    (let
        ((c 0))
        (lambda (x)
            (if (= c 0)
                (begin
                    (set! c 1) x)
                0))))