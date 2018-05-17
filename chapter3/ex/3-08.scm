(define f
  (let ((flag 1)
        (ret 0))
    (lambda (x)
      (if (= flag 1)
        (begin
          (set! flag 0)
          (set! ret x)
          ret)
        ret))))

(print (f 1)) ; 1
(print (f 0)) ; 1

(define g f)
(print (g 0)) ; 1
(print (g 1)) ; 1

