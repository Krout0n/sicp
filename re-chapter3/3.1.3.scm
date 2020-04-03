(define (make-simplified-withdraw balance)
    (lambda (amount)
        (set! balance (- balance amount ))
        balance))

(define W (make-simplified-withdraw 25))
(define (make-decrementer balance)
    (lambda (amount)
    (- balance amount )))

(define D (make-decrementer 25))

((make-decrementer 25) 20)
; ((lambda (amount) (- 25 amount )) 20)
; (-25 20)

; ((make-simplified-withdraw 25) 20)
; ((lambda (amount) (set! balance (- 25 amount )) 25) 20)
; ((set! balance (-25 20) ))

