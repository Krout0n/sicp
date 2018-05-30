; (make-rat ⟨n⟩ ⟨d⟩) は、分⼦が整数 ⟨n⟩ で分⺟が整数 ⟨d⟩ である有理数を返す。
; (numer ⟨x⟩) は、有理数 ⟨x⟩ の分⼦を返す。
; (denom ⟨x⟩) は、有理数 ⟨x⟩ の分母を返す。
(define (add-rat x y)
    (make-rat
        (+  (* (numer x) (denom y))
            (* (numer y) (denom x)))
        (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat
        (-  (* (numer x) (denom y))
            (* (numer y) (denom x)))
        (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat
        (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat
        (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y)
    (=
        (* (numer x) (denom y)) (* (denom x) (numer y))))

(define x (cons 1 2))
(car x) ; 1
(cdr x) ; 2
(define x (cons 1 2)) 
(define y (cons 3 4))
(define z (cons x y))
(car (car z)) ; 1
(cdr (car z)) ; 3
; これらのcons, car, cdrを用いて有理数を表現したい！！！！
(define (make-rat x y) (cons x y))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (gcd a b)
    (if (= b 0)
        a
    (gcd b (remainder a b))))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))
