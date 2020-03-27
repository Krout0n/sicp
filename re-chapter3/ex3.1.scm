(define (make-accumlator initial-value)
    (lambda (arg)
        (set! initial-value (+ initial-value arg))
        initial-value))

(define A (make-accumlator 5))
(define B (make-accumlator 5))

(define (test obj val expected)
    (display (if (= (obj val) expected) "" "満足できねえぜ")))

(test A 10 15)
(test A 10 25)
(test B 3 8)
(test B 4 12)
