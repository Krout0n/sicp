; ex3.7
(define (make-account balance password)
    
    (define (withdraw amount)
        (if (>= balance amount)
            (begin
                (set! balance (- balance amount))
                balance)
            "Insufficient funds"
        )
    )
    
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    
    (define (dispatch p m)
        (if (eq? p password)
            (cond
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else
                    (error "Unknown request: MAKE-ACCOUNT") dispatch)
            )
            (error "Incorrenct password")
        )
    )
)

(define (make-joint acc password joint-password)
    (define (dispatch key m)
        (if (eq? key joint-password)
            (cond
                ((eq? m 'withdraw) (acc password 'withdraw))
                ((eq? m 'deposit) (acc password 'deposit))
                (else
                    (error "Unknown request: MAKE-JOINT") dispatch)
            )
            (error "Incorrenct password")
        )
    )
)

; (define peter-acc (make-account 100 'open-sesame))
; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
 
; ((peter-acc 'open-sesame 'withdraw) 10)
; 90
 
; ((paul-acc 'rosebud 'withdraw) 10)
; 80

;ex3.8 よくわからないのでパス