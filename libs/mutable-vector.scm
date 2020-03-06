(define-library (sicp mutable-vector)
  (export make-mutable-vector
          mutable-vector?
          add-element
          push-to-front
          remove-first)
  (import (scheme base))
  (begin
    (define-record-type mutable-vector
      (make-mutable-vec used buff capacity)
      mutable-vector?
      (used get-used set-used!)
      (buff get-buff set-buff!)
      (capacity get-capacity set-capacity!))
    
    (define (make-mutable-vector . initial-capacity)      
      (cond ((null? initial-capacity)
             (make-mutable-vec 0 (make-vector 1) 1))
            (else (let (initial-capacity (car initial-capacity))
                    (if (argument-is-positive-integer initial-capacity)
                        (make-mutable-vec 0 (make-vector initial-capacity) initial-capacity)
                        (error "invalid argument for initial vector capacity" initial-capacity))))))

    (define (add-element mutable-vec element)
      (let ((used (get-used mutable-vec))
            (capacity (get-capacity mutable-vec)))))

    (define (push-to-front mutable-vec element)
      mutable-vec)
    
    (define (remove-first mutable-vec)
      mutable-vec)

    (define (remove-at mutable-vec index)
      mutable-vec)

    (define (insert-at mutable-vec index element)
      mutable-vec)

    (define (argument-is-positive-integer? arg)
      (and (integer? initial-size)
           (> initial-size 0)))))
