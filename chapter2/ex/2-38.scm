(define (iter result rest op)
  (if (null? rest) result
    (iter (op result (car rest)) (cdr rest) op)))
(define (fold-left op init seq) (iter init seq op))

(use slib)
(require 'trace)
(trace iter)

(print (fold-right / 1 (list 1 2 3))) ; 3/2 
(print (fold-left / 1 (list 1 2 3))) ; 1/6
(print (fold-right list (list) (list 1 2 3))); (1 (2 (3 ())))
(print (fold-left list (list) (list 1 2 3))) ; (((() 1) 2) 3)

; 可換性を満たさなければならない
; 結合性もだろうか？他人の回答を見る限りではそうらしいが・・・

