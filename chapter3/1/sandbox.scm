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

; 乱数めんどそうだから飛ばす

(define (make-simplified-withdraw balance)
    (lambda (amount)
        (set! balance (- balance amount))
    )
)

; gosh> (define D (make-simplified-withdraw 20))
; D
; gosh> (D 10)
; 10
; gosh> (D 5)
; 5
; gosh> (D 10)
; -5

(define (make-decrementer balance)
    (lambda (amount)
        (- balance amount)
    )
)

; gosh> (define D (make-decrementer 20))
; D
; gosh> (D 10)
; 10
; gosh> (D 5)
; 15
; gosh> (D 10)
; 10

; 参照透過性がないとこういう代入したときに辛いね〜って話

; gosh> (define peter-acc (make-account 100))
; peter-acc
; gosh> (define paul-acc peter-acc)
; paul-acc
; gosh> ((peter-acc 'withdraw) 50)
; 50
; gosh> ((paul-acc 'withdraw) 50)
; 0

; paul-accとpeter-accは別物として扱われて欲しい，それはそう

; 命令型は辛いよって話
; 関数型の階乗
(define (factorial n)
    (define (iter product counter)
        (if (> counter n) product
            (iter (* product counter) (+ counter 1))
        )
    )
    (iter 1 1)
)

(define (factorial n)
    (let
        (
            (product 1)
            (counter 1)
        )
        (define (iter)
            (if (> counter n) product
                (begin
                    (set! product (* product counter))
                    (set! counter (+ counter 1))
                )
            )
            (iter)
        )
    (iter))
)