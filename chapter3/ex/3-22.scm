(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (front-queue)
      (if (empty-queue?) (error "baka")
        (car front-ptr)))

    (define (empty-queue?)
      (null? front-ptr))

    (define (set-front-ptr! item)
      (set! front-ptr item)
      front-ptr)

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue?)
          (begin
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair))
          (set-cdr! rear-ptr new-pair))))

    (define (delete-queue!)
      (if (empty-queue?)
        (error "YOU TRIED TO CALL delete-queue! FOR EMPTY QUEUE")
        (begin
          (set-front-ptr! (cdr front-ptr))
          (cons front-ptr rear-ptr))))

    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "unexpected message: " m))))
      dispatch))

(define q (make-queue))

((q 'insert-queue!) 10)
((q 'insert-queue!) 30)
(print ((q 'front-queue)))
((q 'delete-queue!))
(print ((q 'front-queue)))
((q 'delete-queue!))

