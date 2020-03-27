; 口座オブジェクトが別れて具合ヨシ！！

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin
                (set! balance (- balance amount))
                balance)
            "Insufficient funds")))

(define mine (make-withdraw 100))
(define yours (make-withdraw 100))
(mine 25) ; 75
(yours 10) ; 90
