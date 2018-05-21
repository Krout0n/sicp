(define call-the-cops (lambda () (print "I WILL ARREST YOU")))

(define (make-account balance password)
  
  (define (withdraw amount)
    (if (>= balance amount) (set! balance (- balance amount))
      "insufficient funds"))

  (define (deposit amount)
    (set! balance (+ amount balance)))

    (let ((counter 0))
      (define (dispatch pw m)
        (if (eq? pw password)
          (begin
            (set! counter 0)
            (cond ((eq? m 'deposit) deposit)
                  ((eq? m 'withdraw) withdraw)
                  (else (error "Unknown dispatch: " m))))
          (begin
            (set! counter (+ counter 1))
            (if (< counter 7)
              (lambda (_) (print "invalid-pw, you missed: " counter))
              (lambda (_) (call-the-cops))))))
      dispatch))

(define W1 (make-account 1000 'password))

((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'password 'withdraw) 100) ; reset
(print "Missed time reset")
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
((W1 'invalid-pw 'withdraw) 9999) ; error 
; invalid-pw, you missed: 1
; invalid-pw, you missed: 2
; invalid-pw, you missed: 3
; invalid-pw, you missed: 4
; Missed time reset
; invalid-pw, you missed: 1
; invalid-pw, you missed: 2
; invalid-pw, you missed: 3
; invalid-pw, you missed: 4
; invalid-pw, you missed: 5
; invalid-pw, you missed: 6
; I WILL ARREST YOU

