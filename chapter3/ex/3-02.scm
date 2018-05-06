(load "../../lib.scm")

(define (make-monitored proc)
  (let ((counter 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls) counter)
            (else (begin
                    (set! counter (+ counter 1))
                    (proc m)))))))

(define s (make-monitored sqrt))
(trace s)
(s 'how-many-calls) ; 0
(s 100) ; 10
(s 'how-many-calls) ; 1

; CALL s how-many-calls
; RETN s 0
; CALL s 100
; RETN s 10
; CALL s how-many-calls
; RETN s 1

