; データ抽象化は，最小責任原則の応用として見ることもできる．
; 2.4.1では直交形式による表現と極形式によるどちらの表現を使うこともできた．
; セレクタとコンストラクタによって抽象化の壁を作っているので，
; データオブジェクトの具体的な表現に何を選ぶかということを最後まで遅らせることができた．
; これによって，システム摂家の柔軟性を最大限にできた．

; 最小責任の原則は，さらに極限まで進めることができる．
; セレクタとコンストラクタの設計した後になっても表現を曖昧にしたままBenとAlyssaの表現を両方使うようにすることもできる．
; しかし，両方の実装が単独のシステムに含まれている場合，BenとAlyssaのデータを区別する何らかの方法がいる．
; そうしないと例えば(3 4)というペアのmagnitudeを求める指示があった場合, Benだと5, Alyssaだと3のどちらにするべきかわからない．
; これを区別するためにタイプタグとして rectangular or polar を使ってやっていく．

; タグ付きデータを操作するためには，type-tagとcontentsという手続きを持っていると想定する．
; type-tagはデータオブジェクトからタグを，contentsが実際の中身を抽出するものだ．
; また，attach-tagという手続きもあると仮定する.

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

; これらの手続きを使って直交形式と極形式を識別する述語 rectangular? と polar? を定義する．

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

; タイプタグを使うことにしたのでBenとAlyssaはコードを修正して異なる表現が同じシステムの中で共存できるようにした，
; Benが複素数を構築するたびに，それに rectangular というタグがつき, Alyssaが構築するたびに polar がつく
; それに加えて，Ben と Alyssa はそれぞれの手続きの名前が衝突しないようにしなければならない．
; これを行う方法としては，Benが自分の表現の手続きのsuffixに rectangular を追加し，Alyssa は polar を追加した．

; 以下はBenの修正したものである．

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular
              (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; 同様にAlyssa
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar x y)
  (attach-tag 'polar
              (cons x y)))

; どのジェネリックセレクタも，引数のタグをチェックして，そのタイプのデータを扱う適切な手続きを呼ぶ手続きとして実装されている．
; 例えば，複素数の実部を得る場合は real-part はタグを調べ, real-part-rectangular か real-part-polar のどっちを使うべきかを決定する．
; どちらの場合でも，contentsを使って生のタグなしデータを取り出し，必要に応じて直交形式または極形式の手続きを呼び出す．

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown-type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown-type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown-type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown-type: angle" z))))

; 複素数の算術演算を実装するのには前に実装した add-complex sub-complex mul-complex div-complex と同じものが使える．
; これらが呼び出すセレクタ(real-part imag-part など)はジェネリックなのでどちらの表現を使っても動くからである．

; 最後に複素数を構築するのにBenとAlyssaのどちらを使うかについてだが，
; 実部と虚部を渡された場合は rectangular, 大さと角度を渡された時には polar を使うのが妥当だろう．

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; システムは，比較的独立した3つの部分に分かれている．
; 1つは 複素数算術演算
; 1つは Ben が作った rectangular
; 1つは Alyssa が作った polar

; rectangular と polar の実装は Ben と Alyssa が別々に書いていたとしても問題がない．
; 第三者のプログラマが抽象コンストラクタ，セレクタによって複素数算術演算手続きを実装する際に，内部表現として両方とも使えるので

; それぞれのデータオブジェクトには型のタグがついていいるので，セレクタ(real-part など)はジェネリックにデータを操作する．
; つまり，それぞれのセレクタは適用されるデータの個々の型によって異なる振る舞いをするように定義されているということである．
; 別々の表現をインターフェースで接続する際の一般的なメカニズムに注目する．
; ある表現の実装(例えば，polarパッケージ)の中でpolarの複素数を操作するときは, タグを取り除き, 中身をAlyssaのコードに渡す．
; 逆にAlyssaが一般使用のために数値を構築する際にはタグをつけて上のレベルの手続きが適切に認識できるようにする．
; データオブジェクトをレベル間でやりとりする際にタグをつけたり外したりするというこの規律は重要な組織化戦略となる．
; 2.5でやっていく

