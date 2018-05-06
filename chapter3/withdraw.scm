(load "../lib.scm")
; もともと口座の中に100あったとして25引き出す
; (withdraw 25) 100 - 25 = 75
; 再度引き出した際に75-nにならないといけないという話
; (withdraw 25) -> 50

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
    (set! balance (- balance amount))
    "insufficient funds"))

(withdraw 10)
(withdraw 90)
(withdraw 100)

; CALL withdraw 10
; RETN withdraw 90
; CALL withdraw 90
; RETN withdraw 0
; CALL withdraw 100
; RETN withdraw "insufficient funds"

; balanceが外部から参照できると他からdefineできたりしてキモいという話

(define (new-withdraw)
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (set! balance (- balance amount))
        "insufficient funds"))))

(define W1 (new-withdraw))
(define W2 (new-withdraw))

(W1 10)
(W1 90)
(W1 20)
(W2 0)

; CALL W1 10
; RETN W1 90
; CALL W1 90
; RETN W1 0
; CALL W1 20
; RETN W1 "insufficient funds"
; CALL W2 0
; RETN W2 100

; できたけどこれじゃ100がデフォの口座しか作れなくて辛いねって話，let式を省いてもっと簡略化できる

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (set! balance (- balance amount))
      "insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 200))
(W1 10)
(W2 20)

; CALL W1 10
; RETN W1 90
; CALL W2 20
; RETN W2 180

; もっと抽象化できる

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount) (set! balance (- balance amount))
      "insufficient funds"))
  (define (deposit amount)
    (set! balance (+ amount balance)))
  (define (dispatch m)
    (cond ((eq? m 'deposit) deposit)
          ((eq? m 'withdraw) withdraw)
          (else (error "Unknown dispatch: " m))))
  dispatch)

(define W1 (make-account 1000))
(print ((W1 'deposit) 30)) ; 1030
(print ((W1 'withdraw) 1000)) ; 30

