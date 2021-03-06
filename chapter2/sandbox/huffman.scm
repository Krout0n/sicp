(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (make-code-tree left right)
  (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree) ; wrapper of symbol-leaf
  (if (leaf? tree) (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree) ; wraaper of weight-tree
  (if (leaf? tree) (weight-leaf tree)
    (cadddr tree)))

