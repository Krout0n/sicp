(load "../lib.scm")

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x tree)
  (cond ((null? tree) #f)
        ((not (pair? tree)) #f)
        ((= (entry tree) x) #t)
        ((< x (entry tree))
         (element-of-set? x (left-branch tree)))
        (else
          (element-of-set? x (right-branch tree)))))

(trace element-of-set?)

(define tree (make-tree 7 (make-tree 3 1 5) (make-tree 9 11 10))) ; 10は存在しないのでmake-treeの引数の数合わせ用
(element-of-set? 10 tree)
; call element-of-set? 10 (7 (3 1 5) (9 11 10))
;  call element-of-set? 10 (9 11 10)
;    call element-of-set? 10 10
;    retn element-of-set? #f
;  retn element-of-set? #f
; retn element-of-set? #f


