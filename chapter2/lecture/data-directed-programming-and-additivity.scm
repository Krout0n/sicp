; データの型をチェックし，適切な手続きを呼ぶ手法は型によるディスパッチと呼ばれるものだ．
; これは，システム設計でモジュール性を達成するのに強力な手法だが，2.4.2節のようにディスパッチを実装することには2つの大きな弱点がある．
; 弱点の1つはジェネリックインターフェース手続き(real-part, imag-part, magnitude, angle)が全ての異なる表現について対応している必要がある
; 例えば，新しい複素数の表現をしてこのシステムに組み込む際に各インターフェースにその新しい型をチェックする部分を追加で実装し
; そしてその表現に適切なセレクタを適用する必要がある.
; もう1つが個々の表現は別々に設計できるものの，システム全体で2つの手続きが同じ名前を持つということがないように保障しなければならないことだ．
; それゆえに Ben と Alyssa は元の手続きの名前を変える必要があった．

; この2つの弱点の元となっているは，ジェネリックインターフェースを実装するテクニックが加法的ではないということである．
; ジェネリックセレクタ手続きを実装する際は，新しい表現が組み込まれるたびにそれらの手続きを変更しなければいけないし
; 名前の衝突を避けるためにコードを変更しなければいけない．
; どっちの場合でも加えるべき変更は単純だが，それでもこれが不便だしエラーの原因にもなる．
; 仮に数百個の表現があって，メンテナンスするべきジェネリックセレクタがめっちゃあったら辛くね？
; そんな時でもいい感じにモジュール化するための手段としてデータ主導プログラミングがある．

; データ主導プログラミングがどうやって動いているかを理解するために次のようなテーブルを考える

;   演算                      型
;           |           polar |           rectangular
; real-part | real-part-polar | real-part-rectangular 
; imag-part | imag-part-polar | imag-part-rectangular 
; magnitude | magnitude-polar | magnitude-rectangular 
; angle     |     angle-polar |     angle-rectangular 

; データ主導プログラミングは，上のようなテーブルを扱うようにプログラムを設計するテクニックだ．
; 以前に，
; - 複素数算術システムのコード
; - それぞれ型によって明示的なディスパッチを行う手続きの集合となっている表現パッケージ(polarとか)
; の橋渡しをするメカニズム(attach-tag, contents)を実装した．
; 今回は，このテーブルに対して put と get という手続きで操作してやっていく場合を考える．
; (put ⟨op⟩ <type> <item>) は テーブルの <op> と <type> が指すところに <item> を入れる
; ex. (put 'magnitude 'polar '(lambda (z) (car z)))?
; (get <op> <item>) は テーブルの <op> と <type> が指すところのオブジェクトを返す，なければ #f
; ex. (get 'magnitude 'polar) -> magnitude-rectangular 

; rectangular を開発した Ben はテーブルに項目を追加するパッケージを実装した．
(define (install-rectangular-package)
   ;; 内部手続き
   (define (real-part z) (car z))
   (define (imag-part z) (cdr z))
   (define (make-from-real-imag x y) (cons x y))
   (define (magnitude z)
     (sqrt (+ (square (real-part z))
              (square (imag-part z)))))
   (define (angle z)
     (atan (imag-part z) (real-part z)))
   (define (make-from-mag-ang r a) 
     (cons (* r (cos a)) (* r (sin a))))
 
    ;; システムの他の部分とのインターフェース
   (define (tag x) (attach-tag 'rectangular x))
   (put 'real-part '(rectangular) real-part)
   (put 'imag-part '(rectangular) imag-part)
   (put 'magnitude '(rectangular) magnitude)
   (put 'angle '(rectangular) angle)
   (put 'make-from-real-imag 'rectangular 
        (lambda (x y) (tag (make-from-real-imag x y))))
   (put 'make-from-mag-ang 'rectangular 
        (lambda (r a) (tag (make-from-mag-ang r a))))
   'done)
 
; 注目するべきは，他の部分と接続する手続きを実装したにもかかわらず
; real-part などそのままの名前を使って実装できている点である
; また，これらの手続きの定義は内部にあるため他のシステムの同名な手続きと衝突しない.
; Benは自分のreal-part手続きを演算名 real-part と型 (rectangular)の所に入れた．
; 他のセレクタも同様にした．
; コンストラクタもBemが内部で定義したものと同じだが，タグをつけるという点が異なる．

; Alyssaのpolar packageも似たようなものになる．
(define (install-polar-package)
   ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

   ;; システムの他の部分とのインターフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; 複素数算術演算セレクタは，apply-genericというジェネリック演算手続きを使ってテーブルにアクセスする．
; これは，ジェネリック演算に引数を適用するものだ．

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))

; apply-generic を用いると複素数演算のジェネリックセレクタは以下のように定義できる．

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; 新しい表現がシステムに追加されてもこれらはまったく影響を受けない．

; また，apply-genericはapplyという基本手続きを使っていて，この手続きは，手続きとリストを引数に取る．
; 例えば (apply + '(1 2 3 4)) は 10になるみたいな
