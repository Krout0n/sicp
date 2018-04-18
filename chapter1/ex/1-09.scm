(define (inc x)(+ x 1))
(define (dec x)(- x 1))
; p as +
(define (p a b)
  (if (= a 0) b (inc (p (dec a) b))))

(print (p 4 5))
; (p 4 5) -> (inc (p (dec 4) 5))
;         -> (inc (p 3 5))
;         -> (inc (inc (p 2 5)))
;         -> (inc (inc (inc (p 1 5))))
;         -> (inc (inc (inc (inc (p 0 5)))))
;         -> (inc (inc (inc (inc 5))))
;         -> (inc (inc (inc 6)))
;         -> (inc (inc 7))
;         -> (inc 8)
;         -> 9

(define (p a b)
  (if (= a 0) b (p (dec a) (inc b))))

(print (p 4 5))

; (p 4 5) -> (p 3 6)
;         -> (p 2 7)
;         -> (p 1 8)
;         -> (p 0 9)
;         -> 9

