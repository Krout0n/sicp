(define (scale-list x term)
  (cond ((null? x) (list))
        ((not (pair? x)) (term x))
        (else (cons (scale-list (car x) term)
                    (scale-list (cdr x) term)))))

(define tester (list (list 1 2) 3 4 (list 5 6 (list 7 8))))
(define (scale x) (* 10 x))
(print (scale-list tester scale))

(define (scale-list x term)
    (map (lambda (y)
           (if (pair? y)
             (scale-list y term)
             (term y))) x))
(use slib)
(require 'trace)
(trace scale-list)
(print (scale-list tester square))

((10 20) 30 40 (50 60 (70 80)))

CALL scale-list ((...) 3 4 (5 6 (7 ...))) #[proc]
  CALL scale-list (1 2) #[proc]
  RETN scale-list (1 4)
  CALL scale-list (5 6 (7 8)) #[proc]
    CALL scale-list (7 8) #[proc]
    RETN scale-list (49 64)
  RETN scale-list (25 36 (49 64))
RETN scale-list ((1 4) 9 16 (25 36 (49 64)))
((1 4) 9 16 (25 36 (49 64)))

