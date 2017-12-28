(define (fizzbuzz n)
    (cond ((= (remainder n 15) 0) "FizzBuzz")
    ((= (remainder n 3) 0) "Fizz")
    ((= (remainder n 5) 0) "Buzz")
    (else n)))

(define (iterative-fizzbuzz n)
    ())

(display (fizzbuzz 15))