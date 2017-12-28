(define (cube x)(* x x x))

(define (sum-integers a b)
    (if (> a b)
        0 (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
    (if (> a b)
        0 (+ (cube a)(sum-cubes (+ a 1) b))))

(define (pi-sum a b)
    (if (> a b)
        0 (+ (/ 1.0 (* a (+ a 2)))(pi-sum (+ a 4) b))))

; 共通部分は下
; (define (<name> a b)
;   (if (> a b)
;       0 (+ (<term> a)(<name> (<next> a) b))))
; これってΣ記法やんけ！！！

(define (sum term a next b)
  (if (> a b)
      0 (+ (term a)(sum term (next a) next b))))

(define (inc n)(+ n 1))
(define (sum-cubes a b)
    (sum cube a inc b))

(define (sp-inc n)(+ n 4))

(define (pi-sum a b)
    (define (calc a)
        (+ (/ 1.0 (* a (+ a 2)))))
    (define (sp-inc n)(+ n 4))
    (sum calc a sp-inc b))

; (define (integral f a b dx)
;     (define add-dx x dx)(+ x dx))
(define (t a result)(+ a result))
(define (n a)(+ a 1))


(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (term a result))))
    (iter a b)
    (trace iter))

(sum t 1 n 10)
