(define (make-from-mag-ang z)
  (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2)))

(define (sub-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (+ square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

; ここまで直交形式による表現, 以下は極形式である．

(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

; 極形式でも直交形式でもデータ抽象化がうまくいってるため計算結果は同じになる・・・はず・・・
; でも例えば(3 4) ってリストがあった時に直交形式なら(3^2 + 4^2)^(1/2)しないといけないし
; 極形式なら3を返す，これじゃどっちかわからないので前回やったクオートを使いタグをつけて区別しようという話

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum) (car datum)
    (error "Bad tagged datum: " datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum)
    (error "Bad tagged datum: " datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)(eq? (type-tag z) 'polar))

; そして直交形式と極形式をそれぞれのwrappedなセレクタに直す

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z)) (+ square (imag-part z)))))
(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag-rectangular x y) (cons x y))
(define (make-from-mag-ang-rectangular r a)
  (cons (* r (cos a)) (* r (sin a))))

; ここまで直交形式による表現, 以下は極形式である．

(define (real-part-polar z) (* (magnitude z) (cos (angle z))))
(define (imag-part-polar z) (* (magnitude z) (sin (angle z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang-polar r a) (cons r a))

; real-part, imag-part, magnitude, angleをどちらのデータが引数として渡されてもいいようにwrapする．

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" (type-tag z)))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" (type-tag z)))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" (type-tag z)))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" (type-tag z)))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

