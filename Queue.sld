(define-library (sicp queues)
  (export make-queue
          queue-pop
          queue-push
          queue-peek
          queue-print
          queue-length
          queue-empty?)
  (import (scheme base))
  (define (make-queue)
    (cons (cons '() '()) 0))
  (define (queue-pop queue)
    (cond ((queue-empty? queue) (error "can't pop an empty queue!"))
          (else (let ((item (front-ptr queue)))
                  (set-front-ptr! queue (cdr (front-ptr queue)))
                  (dec-queue-count! queue)
                  (car item)))))
  (define (queue-push queue item)
    (let ((new-item (cons item '())))
      (cond ((queue-empty? queue) (set-front-ptr! queue new-item)
             (set-rear-ptr! queue new-item)
             (inc-queue-count! queue))
            (else (set-cdr! (rear-ptr queue) new-item)
                  (set-rear-ptr! new-item)
                  (inc-queue-count! queue)))))
  (define (queue-peek queue)
    (cond ((empty-queue? queue) (error "can't peek an empty queue!"))
          (else (car (front-ptr queue)))))
  (define (queue-print queue) '())
  (define (queue-length queue) (car (cdr queue)))
  (define (queue-empty? queue) (= (queue-length queue) 0))
  ;;non-exported methods
  (define (set-front-ptr! queue item) (set-car! (car queue) item))
  (define (set-rear-ptr! queue item) (set-cdr! (car queue) item))
  (define (front-ptr queue) (car (car queue)))
  (define (rear-ptr queue) (cdr (car queue)))
  (define (queue-count queue) cdr queue)
  (define (inc-queue-count! queue) (set-cdr! (+ 1 (queue-count queue))))
  (define (dec-queue-count! queue) (set-cdr! (- (queue-count queue) 1))))
 
