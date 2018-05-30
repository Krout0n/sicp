; 有理数と同じように複素数は順序付きペアとして表現できる．

; また複素数の演算を下に示す
; 実部(z1+z2) = 実部(z1) + 実部(z2)
; 虚部(z1+z2) = 虚部(z1) + 虚部(z2)
; 大きさ(z1+z2) = 大きさ(z1) * 大きさ(z2)
; 角度(z1+z2) = 角度(z1) + 角度(z2)

; つまり，複素数には2つの異なる表現があり，それぞれ異なる演算に適している．
; しかし，複素数を使うプログラムを書いている人間の視点からは，データ抽象化の原則によりコンピュータがどちらの表現を使っていても
; 複素数の演算全てが使えるようになっていなければならないのでやっていくぞい

; 複素数演算はreal-part imag-part magnitude angle という4つのセレクタによって実装されているとする．
; また，複素数のコンストラクタにはmake-from-real-imagとmake-from-mag-angの2つがあり，
; introで書いたようなジェネリックな算術演算は(add-complex sub-complex mul-complex div-complex)である．

; (make-from-real-imag (real-part z) (imag-part z)) と (make-from-mag-ang (magnitude z) (angle z)) は等しい複素数を返すという性質を持っている．

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; 複素数パッケージを完成させるには，複素数の表現を選び，コンストラクタとセレクタを基本数値の基本リストを使って実装しなければいけない．

; 以下は直交形式で複素数を表現している人間のセレクタとコンストラクタである．

(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

; もう一方は極形式で表現することにした．

(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang x y) (cons x y))

; データ抽象化の規律に従っているので，add-complex sub-complex mul-complex div-complex の同じ実装がどちらの表現に対してもうまく動くことが保証されている．


