(load "../../lib.scm") ; trace

(define (make-accumulator n)
  (lambda (m) (set! n (+ m n))))

(define A (make-accumulator 5))
(trace A)
(A 10)
(A 20)
(A -20)

; CALL A 10
; RETN A 15
; CALL A 20
; RETN A 35
; CALL A -20
; RETN A 15
