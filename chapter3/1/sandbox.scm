; (withdraw 25) -> 75
; (withdraw 25) -> 50
; というふうに状態を持った手続きを考える．

(define balance 100)
(define (withdraw amount)
    (if (>= balance amount)
        (begin
            (set! balance (- balance amount))
            balance)
        "Insufficient funds"))
(withdraw 25) ; 75
(withdraw 25) ; 50
(define balance 1000)
(withdraw 100) ; 900
; キモくない？balanceという値にアクセスできる関数はwithdrawだけにしたい...
(define (new-withdraw amount)
    (let
        ((balance 100))
        (lambda (amount)
            (if (>= balance amount)
                (begin
                    (set! balance (- balance amount))
                    balance)
                    "Insufficient funds")
        )
    )
)

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin
                (set! balance (- balance amount))
                balance)
                "Insufficient funds")
    )
)

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 200))
(define (make-account balance)
    
    (define (withdraw amount)
        (if (>= balance amount)
            (begin
                (set! balance (- balance amount))
                    balance)
                    "Insufficient funds")
    )
    
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    
    (define (dispatch m)
        (cond
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            (else
                (error "Unknown request: MAKE-ACCOUNT") dispatch)
        )
    )
)
