(define (list-ref l n)
  (define (iter i r)
    (if (= i n)(car r)
      (iter (+ i 1) (cdr r))))
  (if (or (< (length l) n) (< n 0)) (error "ほげ")
  (iter 0 l)))

(define (count-change amount l)
  (cc amount l))

(define (no-more? coins)
  (if (= (length coins) 0) #t
     #f))

(define (except-first-denomination coins)
  (cdr coins))

(define (first-denomination coins)
  (car coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values)))))

(print (count-change 100 (list 50 25 10 5 1)))
(print (count-change 100 (list 100 50 20 10 5 2 1 0.5)))

