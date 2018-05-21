; make-queue -> 空のキューを返す
; empty-queue? <queue> -> キューが空かどうか判断する
; front-queue <queue> -> キューの先頭にあるオブジェクトを返す．キューが空ならエラーを返す．キュー自体に変更は与えない
; insert-queue! <queue> <item> -> キューの末尾にアイテムを挿入し，変更されたキューを値として返す．
; delete-queue! <queue> -> キューの先頭のアイテムを削除し変更されたキューを値として返す．削除前にキューが空ならエラーを返す．

; (define q (make-queue))
; (insert-queue! q 'a) ; a 
; (insert-queue! q 'b) ; a b
; (delete-queue! q) ; b
; (insert-queue! q 'c) ; b c
; (insert-queue! q 'd) ; b c d
; (delete-queue! q) ; c d

(define (front-ptr queue)
  (car queue))

(define (rear-queue queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-queue queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "DELETE! called with an empty queue" queue)
    (begin
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))

