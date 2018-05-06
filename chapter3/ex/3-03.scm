(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount) (set! balance (- balance amount))
      "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ amount balance)))
  (define (dispatch pw m)
    (if (eq? pw password)
      (cond ((eq? m 'deposit) deposit)
            ((eq? m 'withdraw) withdraw)
            (else (error "Unknown dispatch: " m)))
      (error "Invalid password")))
  dispatch)

(define W1 (make-account 1000 'password))

(print ((W1 'password 'deposit) 30)) ; 1030
(print ((W1 'password 'withdraw) 1000)) ; 30
(print ((W1 'invalid-pw 'withtdraw) 9999)) ; error 

