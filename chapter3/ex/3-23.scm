(define (make-dequeue)
  (let ((front-ptr '())
        (rear-prev-ptr '())
        (rear-ptr '()))

    (define (front-deque)
      (if (null? front-ptr) (rear-deque))
        (car front-ptr))

    (define (rear-deque)
        (car rear-ptr))

    (define (empty-deque?)
      (or (null? front-ptr) (null? rear-ptr)))

    (define (set-front-ptr! item)
      (set! front-ptr item))
    
    (define (set-rear-prev-ptr! item)
      (set! rear-prev-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    ; wrapped proc
    (define (init-insert new-pair)
      (set-front-ptr! new-pair)
      (set-rear-prev-ptr! new-pair)
      (set-rear-ptr! new-pair))

    (define (front-insert-deque! item)
        (if (empty-deque?)
          (init-insert (cons item '()))
          (set-front-ptr! (cons item '()))))

    (define (rear-insert-deque! item)
        (if (empty-deque?)
          (init-insert (cons item '()))
          (begin
            (set-rear-prev-ptr! rear-ptr)
            (set-rear-ptr! (cons item '())))))

    (define (front-delete-deque!)
      (set-front-ptr! (cdr front-ptr)))

    (define (rear-delete-deque!)
      (set-rear-ptr! rear-prev-ptr))

    (define (dispatch m)
      (cond ((eq? m 'front-deque) (front-deque))
            ((eq? m 'rear-deque) (rear-deque))
            ((eq? m 'front-insert-deque!) front-insert-deque!)
            ((eq? m 'rear-insert-deque!) rear-insert-deque!)
            ((eq? m 'front-delete-deque!) (front-delete-deque!))
            ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
            ((eq? m 'print-deque) (print-deque))
            (else (error "UNEXPECTED OPERATION" m))))
    dispatch))

(define d (make-dequeue))

(define (f-i! item)
  ((d 'front-insert-deque!) item))

(define (r-i! item)
  ((d 'rear-insert-deque!) item))

(define (f-d!)
  (d 'front-delete-deque!))

(define (r-d!)
  (d 'rear-delete-deque!))

(f-i! 'x) ; (x)
(r-i! 'z) ; (x z)
(f-i! 'a) ; (a x z)
(print (d 'front-deque))
(print (d 'rear-deque))

(r-d!) ; (a x)
(print (d 'front-deque)) ; a
(print (d 'rear-deque)) ; x

(f-d!)
