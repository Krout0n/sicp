(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define t a-plus-abs-b)
(t 1 2) ; 3
(t 3 4) ; 7
(t 1 -2) ; 3
(t 3 -4) ; 7
(t -3 4); 1
