; これじゃ口座みんな一緒だしwithdrawがグローバルに依存していて気持ち悪い
(define balance 100)
(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
            balance)
        "Insufficient funds"))

(withdraw 25)
(withdraw 25)
