(define (factorial n)
    (if (= n 1)
        1
    (* n (factorial (- n 1)))))
(print (factorial 6)) ; 720

(define (factorial n)
    (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
    (if > counter max-count)
        product
    (fact-iter ((* product counter) (+ product 1) max-count)))