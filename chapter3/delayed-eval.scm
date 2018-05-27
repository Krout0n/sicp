(load "../lib.scm") ; prime? accumulate

(define (sum-prime a b)
  (define (iter n p)
    (cond ((< b n) p)
          ((prime? n) (iter (+ n 1) (+ n p)))
          (else (iter (+ n 1) p))))
  (iter a 0))

(print (sum-prime 2 20)) ; 77

(define (sum-prime a b)
  (accumulate + 0 (filter prime? (enumerate-intervals a b))))

(print (sum-prime 2 20)) ; 77

; これらの計算は (2 3 5 7 11 13 15 17 19) を 生成してから行われる．
; (enumerate-intervals 10000 10000000)の区間に含まれる2番目の素数を計算するのに
; (car (cdr (filter prime? (enumerate-intervals 10000 10000000))))
; として呼び出せるが，実行時にかなりのオーバーヘッドを伴うことになる．

; ストリームは，これらの生成のコストを発生させずに列操作ができるようになる神
; そこで コンストラクタ cons-stream, 2つのセレクタ stream-car, stream-cdr があり，セレクタは以下の制約を満たしているものとする．

; (stream-car (cons-stream x y)) = x
; (stream-cdr (cons-stream x y)) = y

; さらに識別可能なオブジェクト the-empty-stream がある. 
; これは決して cons-stream 演算の結果になることがなく stream-null? によって識別可能．
; streamは第2章で学んだlist-ref, map, for-eachのようなリスト演算についてストリーム版を構築ができる

(define (stream-ref stream n)
  (if (= n 0) (stream-car stream)
    (stream-ref (stream-cdr (cdr stream) (- n 1)))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc s)
                 (stream-map proc (cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; delayという特殊形式に基づいてストリームを実装する．
; (delay <exp>)を評価しても，<exp>は評価されずに代わりに遅延オブジェクトを生成して返す．
; cons-stream は delay を用いて次のように表現できる．(cons <a> (delay <b>))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(stream-car
  (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-intervals 10000 10000000))))



