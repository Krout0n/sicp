; これじゃ口座みんな一緒だしwithdrawがグローバルに依存していて気持ち悪い

(define new-withdraw
    (let ((balance 100))
        (lambda (amount)
            (if (>= balance amount)
                (begin
                    (set! balance (- balance amount))
                    balance)
                "Insufficient funds"))))

(define mine new-withdraw)
(define yours new-withdraw)
(mine 25) ; 75
(yours 10) ; 65
