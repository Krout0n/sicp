; ex3-1
(define (make-accumulator n)
    (lambda (m)
        (set! n (+ m n))
        n
    )
)

; gosh> (define A (make-accumulator 5))
; A
; gosh> (A 10)
; 15
; gosh> (A 10)
; 25

; ex3-2
(define (increment x) (+ x 1)) ; sqrtの代わり

(define (make-monitored p)
    (let
        ((counter 0))
        (define (dispatch m)
            (if (eq? m 'how-many-calls?) counter
                (proc m))
        )
        (define (proc arg)
            (set! counter (+ counter 1))
            (p arg)
        )
    dispatch)
)

; gosh> (define s (make-monitored increment))
; s
; gosh> (s 100)
; 101
; gosh> (s 'how-many-calls?)
; 1
; gosh> (s 102)
; 103
; gosh> (s 'how-many-calls?)
; 2

; ex3-3
(define (make-account balance password)
    
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
    
    (define (dispatch p m)
        (if (eq? p password)
            (cond
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                (else
                    (error "Unknown request: MAKE-ACCOUNT") dispatch)
            )
            (error "Incorrenct password")
        )
    )
)

; gosh> (define acc (make-account 100 's))
; acc
; gosh> ((acc 's 'withdraw) 40)
; 60
; gosh> ((acc 'p 'withdraw) 20)
; *** ERROR: Incorrenct password
; Stack Trace:
; _______________________________________
;   0  (acc 'p 'withdraw)
;         at "(standard input)":159
;   1  (eval expr env)
;         at "/usr/local/Cellar/gauche/0.9.5/share/gauche-0.9/0.9.5/lib/gauche/interactive.scm":282
; gosh> ((acc 's 'deposit) 40)
; 100
; gosh>

;ex3-4
(define (make-account balance password)
    (let
        ((counter 0))
        (define (dispatch p m)
            (if (eq? p password)
                (cond
                    ((eq? m 'withdraw) withdraw)
                    ((eq? m 'deposit) deposit)
                    ((eq? m 'balance) balance)
                    (else
                        (error "Unknown request: MAKE-ACCOUNT") dispatch)
                )
                (begin
                    (set! counter (+ counter 1))
                    (display "counter: ")
                    (display counter)
                    (newline)
                    (if (> counter 7)
                        (begin
                            (display "call-the-pop")
                            (newline)
                        )
                        (begin
                            (display "you sent wrong password")
                            (newline)
                        )
                    )
                    (error "Incorrenct password")
                )
            )
        )
        (define (withdraw amount)
            (if (>= balance amount)
                (begin
                    (set! balance (- balance amount))
                    balance)
                        "Insufficient funds")
        )
        dispatch
    )
)

; gosh> (define secure-acc (make-account 100 'pass))
; secure-acc
; gosh> ((secure-acc 'pass 'withdraw) 100)
; 0
; gosh> ((secure-acc 'incorrect 'withdraw) 100)
; counter: 1
; you sent wrong password
; *** ERROR: Incorrenct password
; Stack Trace:
; _______________________________________
;   0  (secure-acc 'incorrect 'withdraw)
;         at "(standard input)":213
;   1  (eval expr env)
;         at "/usr/local/Cellar/gauche/0.9.5/share/gauche-0.9/0.9.5/lib/gauche/interactive.scm":282
; gosh> ((secure-acc 'incorrect 'withdraw) 100)
; counter: 2
; you sent wrong password
; *** ERROR: Incorrenct password
; ...
; ...
; ...
; gosh> ((secure-acc 'incorrect 'withdraw) 100)
; counter: 8
; call-the-pop
; *** ERROR: Incorrenct password
; Stack Trace:
; _______________________________________
;   0  (secure-acc 'incorrect 'withdraw)
;         at "(standard input)":213
;   1  (eval expr env)
;         at "/usr/local/Cellar/gauche/0.9.5/share/gauche-0.9/0.9.5/lib/gauche/interactive.scm":282