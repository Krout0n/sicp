(define (make-account balance correct-password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin
                (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch input-password m)
        (if (eq? input-password correct-password)
            (cond
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'confirm) balance)
                (else (error "Unknown request: MAKE-ACCONT:" m)))
            (lambda (_) "Incorrect password")))
    dispatch)

(define mine (make-account 100 'a))

((mine 'a 'withdraw) 10) ; 90
((mine 'b 'withdraw) 10) ; "Incorrect password"
